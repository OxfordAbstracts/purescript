{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.PureScript.Lsp.Rebuild (RebuildResult (..), rebuildFile, buildExportEnvCacheAndHandleErrors, codegenTargets) where

import Control.Concurrent.STM (TVar)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Map.Lazy qualified as M
import Data.Set qualified as Set
import Language.LSP.Protocol.Types (NormalizedUri, fromNormalizedUri, uriToFilePath)
import Language.LSP.Server (MonadLsp, getConfig)
import Language.PureScript (ExternsFile, primEnv)
import Language.PureScript.AST qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Environment qualified as P
import Language.PureScript.Errors qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Ide.Rebuild (updateCacheDb)
import Language.PureScript.Lsp.Cache (selectDependencies, selectExternsCount)
import Language.PureScript.Lsp.Log (debugLsp, errorLsp, logPerfStandard, warnLsp)
import Language.PureScript.Lsp.ReadFile (lspReadFileText)
import Language.PureScript.Lsp.ServerConfig (ServerConfig (outputPath), getInferExpressions, getMaxFilesInCache)
import Language.PureScript.Lsp.State (addExternsToExportEnv, buildExportEnvCache, cacheEnvironment, cacheRebuild', cachedEnvironment, getDbConn, mergeExportEnvCache, updateCachedModule)
import Language.PureScript.Lsp.Types (ExternDependency (edExtern), LspEnvironment (lspStateVar), LspState)
import Language.PureScript.Lsp.Types qualified as Types
import Language.PureScript.Make qualified as P
import Language.PureScript.Make.Index (addAllIndexing)
import Language.PureScript.Names qualified as P
import Language.PureScript.Options qualified as P
import Language.PureScript.Sugar.Names qualified as P
import Language.PureScript.TypeChecker qualified as P
import Protolude hiding (moduleName, race, race_, threadDelay)

rebuildFile ::
  forall m.
  ( MonadThrow m,
    MonadReader Types.LspEnvironment m,
    MonadLsp ServerConfig m
  ) =>
  NormalizedUri ->
  m RebuildResult
rebuildFile uri = do
  logPerfStandard ("Rebuilt file: " <> show (fold $ uriToFilePath $ fromNormalizedUri uri)) do
    fp <- case fromNormalizedUri uri & uriToFilePath of
      Just x -> pure x
      Nothing -> throwM $ CouldNotConvertUriToFilePath uri
    input <- lspReadFileText uri
    case sequence $ CST.parseFromFile fp input of
      Left parseError ->
        pure $ RebuildError $ CST.toMultipleErrors fp parseError
      Right (pwarnings, m) -> do
        debugLsp $ "Rebuilding module: " <> show (P.runModuleName $ P.getModuleName m)
        updateCachedModule m
        let moduleName = P.getModuleName m
        let filePathMap = M.singleton moduleName (Left P.RebuildAlways)
        outputDirectory <- outputPath <$> getConfig
        conn <- getDbConn
        stVar <- asks lspStateVar
        maxCache <- getMaxFilesInCache
        let mkMakeActions :: Map P.ModuleName FilePath -> P.MakeActions P.Make
            mkMakeActions foreigns =
              P.buildMakeActions outputDirectory filePathMap foreigns False
                & addAllIndexing conn
                & addRebuildCaching stVar maxCache
        externDeps <- logPerfStandard "Selected dependencies" $ selectDependencies m
        when (null externDeps) do
          warnLsp $ "No dependencies found for module: " <> show moduleName
          checkExternsExist
        let externs = fmap edExtern externDeps
        foreigns <- P.inferForeignModules (M.singleton moduleName (Right fp))
        (exportEnv, env) <- logPerfStandard "built export cache" $ getEnv fp externDeps
        ideCheckState <- getIdeCheckState
        (res, warnings) <- logPerfStandard "Rebuilt Module" $ liftIO $ do
          P.runMake (P.defaultOptions {P.optionsCodegenTargets = codegenTargets}) do
            newExtern <- P.rebuildModuleWithProvidedEnv ideCheckState Nothing (mkMakeActions foreigns) exportEnv env externs m Nothing
            updateCacheDb codegenTargets outputDirectory fp Nothing moduleName
            pure newExtern
        debugLsp $ "Rebuild success: " <> show (isRight res)
        case res of
          Left errs -> pure $ RebuildError errs
          Right _ -> do
            pure $ RebuildWarning (CST.toMultipleWarnings fp pwarnings <> warnings)
        where
          checkExternsExist = do
            externCount <- selectExternsCount
            when (externCount == 0) do
              errorLsp "No externs found in database, please build project"

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

buildExportEnvCacheAndHandleErrors ::
  (MonadReader Types.LspEnvironment m, MonadLsp ServerConfig m, MonadThrow m) =>
  m [ExternDependency] ->
  P.Module ->
  [ExternsFile] ->
  m (P.Env, Maybe [ExternDependency])
buildExportEnvCacheAndHandleErrors refetchExterns m externs = do
  fromCache <- buildExportEnvCache m externs
  case fromCache of
    Left err -> do
      warnLsp $ "Error building export env cache: " <> show err
      externs' <- refetchExterns
      envRes <- addExternsToExportEnv primEnv $ edExtern <$> externs'
      case envRes of
        Left err' ->
          throwM $
            CouldNotRebuildExportEnv $
              P.prettyPrintMultipleErrors P.noColorPPEOptions err'
        Right env -> do
          mergeExportEnvCache env
          pure (env, Just externs')
    Right env -> pure (env, Nothing)

data RebuildResult
  = RebuildError P.MultipleErrors
  | RebuildWarning P.MultipleErrors

data RebuildException
  = CouldNotConvertUriToFilePath NormalizedUri
  | CouldNotRebuildExportEnv [Char]
  deriving (Exception, Show)

codegenTargets :: Set P.CodegenTarget
codegenTargets = Set.fromList [P.JS, P.CoreFn, P.Docs]

addRebuildCaching :: TVar LspState -> Int -> P.MakeActions P.Make -> P.MakeActions P.Make
addRebuildCaching stVar maxCache ma =
  ma
    { P.codegen = \prevEnv checkSt astM m docs ext -> lift (liftIO $ cacheRebuild' stVar maxCache ext (P.checkIdeArtifacts checkSt) astM) <* P.codegen ma prevEnv checkSt astM m docs ext
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
