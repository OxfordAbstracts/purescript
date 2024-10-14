{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.PureScript.Lsp.Rebuild (RebuildResult (..), rebuildFile, codegenTargets) where

import Control.Concurrent.STM (TVar)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Map.Lazy qualified as M
import Data.Set qualified as Set
import Language.LSP.Protocol.Types (NormalizedUri, fromNormalizedUri, uriToFilePath)
import Language.LSP.Server (MonadLsp (getLspEnv))
import Language.PureScript (MultipleErrors)
import Language.PureScript.AST qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Errors qualified as P
import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.Ide.Rebuild (updateCacheDb)
import Language.PureScript.Lsp.Cache (selectDependencies)
import Language.PureScript.Lsp.Log (debugLsp, logPerfStandard)
import Language.PureScript.Lsp.ReadFile (lspReadFileText)
import Language.PureScript.Lsp.ServerConfig (ServerConfig (outputPath), getMaxFilesInCache)
import Language.PureScript.Lsp.State (addExternToExportEnv, buildExportEnvCache, cacheRebuild', cachedRebuild)
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig, lspDbConnection, lspStateVar), LspState, OpenFile (OpenFile))
import Language.PureScript.Make qualified as P
import Language.PureScript.Make.Index (addAllIndexing)
import Language.PureScript.Options qualified as P
import Protolude hiding (moduleName)

rebuildFile ::
  ( MonadThrow m,
    MonadReader LspEnvironment m,
    MonadLsp ServerConfig m
  ) =>
  NormalizedUri ->
  m RebuildResult
rebuildFile uri = logPerfStandard "Rebuild file " do
  fp <- case fromNormalizedUri uri & uriToFilePath of
    Just x -> pure x
    Nothing -> throwM $ CouldNotConvertUriToFilePath uri
  input <- lspReadFileText uri
  case sequence $ CST.parseFromFile fp input of
    Left parseError ->
      pure $ RebuildError $ CST.toMultipleErrors fp parseError
    Right (pwarnings, m) -> do
      let moduleName = P.getModuleName m
      let filePathMap = M.singleton moduleName (Left P.RebuildAlways)
      outputDirectory <- asks (confOutputPath . lspConfig)
      conn <- asks lspDbConnection
      stVar <- asks lspStateVar
      maxCache <- getMaxFilesInCache
      cachedBuild <- cachedRebuild fp
      let makeEnv foreigns externs =
            P.buildMakeActions outputDirectory filePathMap foreigns False
              & shushProgress
              & addAllIndexing conn
              & addRebuildCaching stVar maxCache externs
      debugLsp $ "Cache found: " <> show (isJust cachedBuild)
      case cachedBuild of
        Just (OpenFile _ _ externs env _) -> do
          foreigns <- P.inferForeignModules (M.singleton moduleName (Right fp))
          exportEnv <- logPerfStandard "build export cache" $ buildExportEnvCache m externs
          res <- logPerfStandard "Rebuild Module with provided env" $ liftIO $ do
            P.runMake (P.defaultOptions {P.optionsCodegenTargets = codegenTargets}) do
              newExtern <- P.rebuildModuleWithProvidedEnv (makeEnv foreigns externs) exportEnv env externs m Nothing
              updateCacheDb codegenTargets outputDirectory fp Nothing moduleName
              pure newExtern
          handleRebuildResult fp pwarnings res
        Nothing -> do
          externs <- logPerfStandard "Select depenencies" $ selectDependencies m
          foreigns <- P.inferForeignModules (M.singleton moduleName (Right fp))
          exportEnv <- logPerfStandard "build export cache" $ buildExportEnvCache m externs
          res <- logPerfStandard "Rebuild Module" $ liftIO $ do
            P.runMake (P.defaultOptions {P.optionsCodegenTargets = codegenTargets}) do
              newExtern <- P.rebuildModule' (makeEnv foreigns externs) exportEnv externs m
              updateCacheDb codegenTargets outputDirectory fp Nothing moduleName
              pure newExtern
          handleRebuildResult fp pwarnings res
  where
    handleRebuildResult fp pwarnings (result, warnings) = do
      case result of
        Left errors ->
          pure $ RebuildError errors
        Right newExtern -> do
          addExternToExportEnv newExtern
          pure $ RebuildWarning (CST.toMultipleWarnings fp pwarnings <> warnings)

data RebuildResult
  = RebuildError P.MultipleErrors
  | RebuildWarning P.MultipleErrors

data RebuildException = CouldNotConvertUriToFilePath NormalizedUri | CouldNotReadCacheDb MultipleErrors
  deriving (Exception, Show)

codegenTargets :: Set P.CodegenTarget
codegenTargets = Set.fromList [P.JS, P.CoreFn, P.Docs]

-- | Shuts the compiler up about progress messages
shushProgress :: (Monad m) => P.MakeActions m -> P.MakeActions m
shushProgress ma =
  ma {P.progress = \_ -> pure ()}

addRebuildCaching :: TVar LspState -> Int -> [ExternsFile] -> P.MakeActions P.Make -> P.MakeActions P.Make
addRebuildCaching stVar maxCache deps ma =
  ma
    { P.codegen = \prevEnv env astM m docs ext -> lift (liftIO $ cacheRebuild' stVar maxCache ext deps prevEnv astM) <* P.codegen ma prevEnv env astM m docs ext
    }
