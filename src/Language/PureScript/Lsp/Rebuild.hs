{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.PureScript.Lsp.Rebuild (rebuildFile, codegenTargets, rebuildFilePathFromUri) where

import Control.Concurrent.STM (TVar)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Map.Lazy qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.LSP.Protocol.Types (NormalizedUri, fromNormalizedUri, uriToFilePath)
import Language.LSP.Server (MonadLsp, getConfig)
import Language.PureScript (ExternsFile)
import Language.PureScript.AST qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Environment qualified as P
import Language.PureScript.Errors qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Ide.Imports (Import (Import), sliceImportSection)
import Language.PureScript.Ide.Rebuild (updateCacheDb)
import Language.PureScript.Lsp.Cache (selectDependencies, selectDependencyHashFromImports, selectExternsCount)
import Language.PureScript.Lsp.Log (debugLsp, errorLsp, logPerfStandard, warnLsp)
import Language.PureScript.Lsp.ReadFile (lspReadFileText)
import Language.PureScript.Lsp.ServerConfig (ServerConfig (outputPath), getInferExpressions, getMaxFilesInCache)
import Language.PureScript.Lsp.State (addExternsToExportEnv, cacheEnvironment, cachedEnvironment, cachedOpenFileFromSrc, getDbConn, hashDeps, updateCachedRebuildResult, mergePartialArtifacts)
import Language.PureScript.Lsp.Types (ExternDependency (edExtern), LspEnvironment (lspStateVar), LspState)
import Language.PureScript.Lsp.Types qualified as Types
import Language.PureScript.Make qualified as P
import Language.PureScript.Make.Index (addAllIndexing)
import Language.PureScript.Names qualified as P
import Language.PureScript.Options qualified as P
import Language.PureScript.Sugar.Names qualified as P
import Language.PureScript.TypeChecker qualified as P
import Protolude hiding (moduleName, race, race_, threadDelay)

rebuildFilePathFromUri :: (MonadThrow m) => NormalizedUri -> m FilePath
rebuildFilePathFromUri uri = case fromNormalizedUri uri & uriToFilePath of
  Just x -> pure x
  Nothing -> throwM $ CouldNotConvertUriToFilePath uri

rebuildFile ::
  forall m.
  ( MonadThrow m,
    MonadReader Types.LspEnvironment m,
    MonadLsp ServerConfig m
  ) =>
  NormalizedUri ->
  m Types.RebuildResult
rebuildFile uri = do
  fp <- rebuildFilePathFromUri uri
  logPerfStandard ("Rebuilt file: " <> T.pack fp) do
    input <- lspReadFileText uri
    cachedRes <- getCachedRebuildResult fp input
    debugLsp $ T.pack fp <> " rebuild cache hit: " <> show (isJust cachedRes)
    case cachedRes of
      Just res -> pure res
      Nothing -> do
        case sequence $ CST.parseFromFile fp input of
          Left parseError ->
            pure $ Types.RebuildError $ CST.toMultipleErrors fp parseError
          Right (pwarnings, m) -> do
            debugLsp $ "Rebuilding module: " <> show (P.runModuleName $ P.getModuleName m)
            externDeps <- logPerfStandard "Selected dependencies" $ selectDependencies m
            let moduleName = P.getModuleName m
                filePathMap = M.singleton moduleName (Left P.RebuildAlways)
                depHash = hashDeps externDeps
            outputDirectory <- outputPath <$> getConfig
            conn <- getDbConn
            stVar <- asks lspStateVar
            maxCache <- getMaxFilesInCache
            let mkMakeActions :: Map P.ModuleName FilePath -> P.MakeActions P.Make
                mkMakeActions foreigns =
                  P.buildMakeActions outputDirectory filePathMap foreigns False
                    & addAllIndexing conn
                    & addRebuildCaching moduleName stVar maxCache input depHash
            when (null externDeps) do
              warnLsp $ "No dependencies found for module: " <> show moduleName
              checkExternsExist
            let externs = fmap edExtern externDeps
            foreigns <- P.inferForeignModules (M.singleton moduleName (Right fp))
            (exportEnv, env) <- logPerfStandard "built export cache" $ getEnv fp externDeps
            ideCheckState <- getIdeCheckState
            (res, warnings) <- logPerfStandard "Rebuilt Module" $ liftIO $ do
              P.runMake (P.defaultOptions {P.optionsCodegenTargets = codegenTargets}) do
                newExtern <- P.rebuildModuleWithProvidedEnv ideCheckState (mkMakeActions foreigns) exportEnv env externs m Nothing
                updateCacheDb codegenTargets outputDirectory fp Nothing moduleName
                pure newExtern

            debugLsp $ "Rebuild success: " <> show (isRight res)
            rebuildRes <- case res of
              Left errs -> pure $ Types.RebuildError errs
              Right _ -> do
                pure $ Types.RebuildWarning (CST.toMultipleWarnings fp pwarnings <> warnings)
            updateCachedRebuildResult fp rebuildRes
            pure rebuildRes
            where
              checkExternsExist = do
                externCount <- selectExternsCount
                when (externCount == 0) do
                  errorLsp "No externs found in database, please build project"

getCachedRebuildResult :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m) => FilePath -> Text -> m (Maybe Types.RebuildResult)
getCachedRebuildResult fp input = do
  file <- cachedOpenFileFromSrc fp input
  file & maybe (pure Nothing) \Types.OpenFile {..} -> do
    case sliceImportSection $ T.lines input of
      Left _ -> pure Nothing
      Right (_, _, imports, _) -> do
        hash' <- selectDependencyHashFromImports $ getImportModuleName <$> imports
        if hash' == ofDepHash
          then do 
            pure ofRebuildResult
          else pure Nothing

getImportModuleName :: Import -> P.ModuleName
getImportModuleName (Import mn _ _) = mn

getEnv ::
  forall m.
  ( MonadThrow m,
    MonadReader Types.LspEnvironment m,
    MonadLsp ServerConfig m
  ) =>
  FilePath ->
  [ExternDependency] ->
  m (P.Env, P.Environment)
getEnv fp deps = do
  cached <- cachedEnvironment fp deps
  debugLsp $ "Export env cache hit: " <> show (isJust cached)
  cached & maybe fetchEnv pure
  where
    externs = edExtern <$> deps
    fetchEnv = do
      exportEnv <- buildExportEnvFromPrim externs
      let env = foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment externs
      cacheEnvironment fp deps exportEnv env
      pure (exportEnv, env)

buildExportEnvFromPrim :: (Foldable t, MonadThrow m) => t ExternsFile -> m P.Env
buildExportEnvFromPrim =
  addExternsToExportEnv P.primEnv
    >=> either (throwM . CouldNotRebuildExportEnv . P.prettyPrintMultipleErrors P.noColorPPEOptions) pure

data RebuildException
  = CouldNotConvertUriToFilePath NormalizedUri
  | CouldNotRebuildExportEnv [Char]
  deriving (Exception, Show)

codegenTargets :: Set P.CodegenTarget
codegenTargets = Set.fromList [P.JS, P.CoreFn, P.Docs]

addRebuildCaching :: P.ModuleName  -> TVar LspState -> Int -> Text -> Int -> P.MakeActions P.Make -> P.MakeActions P.Make
addRebuildCaching modName stVar _maxCache _src _depHash ma =
  ma
    { 
    --   P.codegen = \prevEnv checkSt astM m docs ext -> lift (P.makeIO "Cache rebuild" $ cacheRebuild' stVar maxCache src ext (P.checkIdeArtifacts checkSt) astM depHash) <* P.codegen ma prevEnv checkSt astM m docs ext
    -- , 
    
    P.withCheckStateOnError = \checkSt -> P.makeIO "replace artifacts" $ mergePartialArtifacts stVar (P.checkIdeArtifacts checkSt) modName
    }

getIdeCheckState :: (MonadLsp ServerConfig m) => m (P.Environment -> P.CheckState)
getIdeCheckState =
  ideCheckState <$> getInferExpressions
  where
    ideCheckState :: Bool -> P.Environment -> P.CheckState
    ideCheckState infer env =
      (P.emptyCheckState env)
        { P.checkAddIdeArtifacts = Just if infer then P.AllIdeExprs else P.IdentIdeExprs
        }
