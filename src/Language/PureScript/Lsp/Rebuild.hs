{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.PureScript.Lsp.Rebuild (RebuildResult (..), rebuildFile, buildExportEnvCacheAndHandleErrors, codegenTargets) where

import Control.Category ((>>>))
import Control.Concurrent.STM (TChan, TVar, writeTChan)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Map.Lazy qualified as M
import Data.Set qualified as Set
import Language.LSP.Protocol.Types (NormalizedUri, fromNormalizedUri, uriToFilePath)
import Language.LSP.Server (MonadLsp, getConfig)
import Language.PureScript (ExternsFile (efModuleName), primEnv)
import Language.PureScript.AST qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Errors qualified as P
import Language.PureScript.Ide.Rebuild (updateCacheDb)
import Language.PureScript.Lsp.Cache (selectDependencies)
import Language.PureScript.Lsp.Log (debugLsp, logPerfStandard, warnLsp)
import Language.PureScript.Lsp.ReadFile (lspReadFileText)
import Language.PureScript.Lsp.ServerConfig (ServerConfig (outputPath), getMaxFilesInCache)
import Language.PureScript.Lsp.State (addExternToExportEnv, addExternsToExportEnv, buildExportEnvCache, cacheDependencies, cacheRebuild', cachedRebuild, getDbConn, mergeExportEnvCache, updateCachedModule, updateCachedModule')
import Language.PureScript.Lsp.Types (ExternDependency (edExtern, edLevel), LspEnvironment (lspStateVar), LspState, OpenFile (OpenFile, ofDependencies))
import Language.PureScript.Make qualified as P
import Language.PureScript.Make.Index (addAllIndexing)
import Language.PureScript.Names qualified as P
import Language.PureScript.Options qualified as P
import Language.PureScript.Sugar.Names qualified as P
import Protolude hiding (moduleName, race, race_, threadDelay)
import Language.PureScript.TypeChecker qualified as P

rebuildFile ::
  forall m.
  ( MonadThrow m,
    MonadReader Language.PureScript.Lsp.Types.LspEnvironment m,
    MonadLsp ServerConfig m
  ) =>
  NormalizedUri ->
  m RebuildResult
rebuildFile uri =
  logPerfStandard "Rebuild module" do
    fp <- case fromNormalizedUri uri & uriToFilePath of
      Just x -> pure x
      Nothing -> throwM $ CouldNotConvertUriToFilePath uri
    input <- lspReadFileText uri
    case sequence $ CST.parseFromFile fp input of
      Left parseError ->
        pure $ RebuildError $ CST.toMultipleErrors fp parseError
      Right (pwarnings, m) -> do
        updateCachedModule m
        let moduleName = P.getModuleName m
        let filePathMap = M.singleton moduleName (Left P.RebuildAlways)
        outputDirectory <- outputPath <$> getConfig
        conn <- getDbConn
        stVar <- asks lspStateVar
        maxCache <- getMaxFilesInCache
        cachedBuild <- cachedRebuild fp
        let mkMakeActions :: Map P.ModuleName FilePath -> [ExternDependency] -> P.MakeActions P.Make
            mkMakeActions foreigns externs =
              P.buildMakeActions outputDirectory filePathMap foreigns False
                -- & broadcastProgress chan
                & addAllIndexing conn
                & addRebuildCaching stVar maxCache externs m
        case cachedBuild of
          Just open -> do
            rebuildFromOpenFileCache fp pwarnings stVar mkMakeActions m open
          Nothing -> do
            rebuildWithoutCache moduleName mkMakeActions fp pwarnings m

rebuildFromOpenFileCache ::
  (MonadLsp ServerConfig m, MonadReader LspEnvironment m, MonadThrow m) =>
  FilePath ->
  [CST.ParserWarning] ->
  TVar LspState ->
  (Map P.ModuleName FilePath -> [ExternDependency] -> P.MakeActions P.Make) ->
  P.Module ->
  OpenFile ->
  m RebuildResult
rebuildFromOpenFileCache fp pwarnings stVar mkMakeActions m (OpenFile moduleName _ externDeps env _ _ _ _) = do
  outputDirectory <- outputPath <$> getConfig
  let externs = fmap edExtern externDeps
  foreigns <- P.inferForeignModules (M.singleton moduleName (Right fp))
  (exportEnv, externsMb) <- logPerfStandard "build export cache" $ buildExportEnvCacheAndHandleErrors (selectDependencies m) m externs
  for_ externsMb (cacheDependencies moduleName)
  res <- logPerfStandard "Rebuild Module with provided env" $ liftIO $ do
    P.runMake (P.defaultOptions {P.optionsCodegenTargets = codegenTargets}) do
      newExtern <- P.rebuildModuleWithProvidedEnv (Just $ updateCachedModule' stVar) (mkMakeActions foreigns externDeps) exportEnv env externs m Nothing
      updateCacheDb codegenTargets outputDirectory fp Nothing moduleName
      pure newExtern
  case fst res of
    Left errs -> debugLsp $ "Rebuild error detected: " <> show errs
    _ -> pure ()
  case fst res of
    Left errs | any couldBeFromNewImports (P.runMultipleErrors errs) -> do
      warnLsp "Module not found error detected, rebuilding without cache"
      rebuildWithoutCache moduleName mkMakeActions fp pwarnings m
    _ -> handleRebuildResult fp pwarnings res

rebuildWithoutCache ::
  (MonadLsp ServerConfig m, MonadReader LspEnvironment m, MonadThrow m) =>
  P.ModuleName ->
  (Map P.ModuleName FilePath -> [ExternDependency] -> P.MakeActions P.Make) ->
  FilePath ->
  [CST.ParserWarning] ->
  P.Module ->
  m RebuildResult
rebuildWithoutCache moduleName mkMakeActions fp pwarnings m = do
  outputDirectory <- outputPath <$> getConfig
  externDeps <- logPerfStandard "Select depenencies" $ selectDependencies m
  let externs = fmap edExtern externDeps
  foreigns <- P.inferForeignModules (M.singleton moduleName (Right fp))
  exportEnv <- logPerfStandard "build export cache" $ addExternsToExportEnvOrThrow primEnv externs
  res <- logPerfStandard "Rebuild Module" $ liftIO $ do
    P.runMake (P.defaultOptions {P.optionsCodegenTargets = codegenTargets}) do
      newExtern <- P.rebuildModule' (mkMakeActions foreigns externDeps) exportEnv externs m
      updateCacheDb codegenTargets outputDirectory fp Nothing moduleName
      pure newExtern
  handleRebuildResult fp pwarnings res

handleRebuildResult :: (MonadLsp ServerConfig f, MonadReader LspEnvironment f) => FilePath -> [CST.ParserWarning] -> (Either P.MultipleErrors ExternsFile, P.MultipleErrors) -> f RebuildResult
handleRebuildResult fp pwarnings (result, warnings) = do
  case result of
    Left errors ->
      pure $ RebuildError errors
    Right newExtern -> do
      addExternToExportEnv newExtern
      pure $ RebuildWarning (CST.toMultipleWarnings fp pwarnings <> warnings)

couldBeFromNewImports :: P.ErrorMessage -> Bool
couldBeFromNewImports =
  P.unwrapErrorMessage >>> \case
    P.ModuleNotFound {} -> True
    P.UnknownImport {} -> True
    P.UnknownImportDataConstructor {} -> True
    P.UnknownName qName | (P.ModName _) <- P.disqualify qName -> True
    _ -> False

cachedImportsAreInActual ::
  ( MonadReader Language.PureScript.Lsp.Types.LspEnvironment m,
    MonadLsp ServerConfig m
  ) =>
  P.Module ->
  OpenFile ->
  m Bool
cachedImportsAreInActual (P.Module _ _ _ decls _) (OpenFile {ofDependencies}) =
  let cachedDirectDeps = Set.fromList $ efModuleName . edExtern <$> filter ((== 1) . edLevel) ofDependencies
      actualDirectDeps =
        Set.fromList $
          decls >>= \case
            P.ImportDeclaration _ importName _ _ -> [importName]
            _ -> []
   in do
        debugLsp $ "Cached direct deps: " <> show (Set.map P.runModuleName cachedDirectDeps)
        debugLsp $ "Actual direct deps: " <> show (Set.map P.runModuleName actualDirectDeps)
        pure $ cachedDirectDeps `Set.isSubsetOf` actualDirectDeps

buildExportEnvCacheAndHandleErrors ::
  (MonadReader Language.PureScript.Lsp.Types.LspEnvironment m, MonadLsp ServerConfig m, MonadThrow m) =>
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

addExternsToExportEnvOrThrow ::
  (MonadReader Language.PureScript.Lsp.Types.LspEnvironment m, MonadLsp ServerConfig m, MonadThrow m) =>
  P.Env ->
  [ExternsFile] ->
  m P.Env
addExternsToExportEnvOrThrow env externs = do
  res <- addExternsToExportEnv env externs
  case res of
    Left err ->
      throwM $
        CouldNotRebuildExportEnv $
          P.prettyPrintMultipleErrors P.noColorPPEOptions err
    Right newEnv -> do
      mergeExportEnvCache newEnv
      pure newEnv

data RebuildResult
  = RebuildError P.MultipleErrors
  | RebuildWarning P.MultipleErrors

data RebuildException
  = CouldNotConvertUriToFilePath NormalizedUri
  | CouldNotRebuildExportEnv [Char]
  deriving (Exception, Show)

codegenTargets :: Set P.CodegenTarget
codegenTargets = Set.fromList [P.JS, P.CoreFn, P.Docs]

-- | Shuts the compiler up about progress messages
-- broadcastProgress :: (MonadLsp ServerConfig m) => TChan P.ProgressMessage -> P.MakeActions P.Make -> m (P.MakeActions P.Make)
broadcastProgress :: (MonadIO m) => TChan (Maybe P.ProgressMessage) -> P.MakeActions m -> P.MakeActions m
broadcastProgress chan ma = do
  ma
    { P.progress = liftIO . atomically . writeTChan chan . Just
    }

addRebuildCaching :: TVar LspState -> Int -> [ExternDependency] -> P.Module -> P.MakeActions P.Make -> P.MakeActions P.Make
addRebuildCaching stVar maxCache deps unchecked ma =
  ma
    { P.codegen = \prevEnv checkSt astM m docs ext -> lift (liftIO $ cacheRebuild' stVar maxCache ext deps prevEnv (P.checkEnv checkSt) checkSt unchecked astM) <* P.codegen ma prevEnv checkSt astM m docs ext
    }
