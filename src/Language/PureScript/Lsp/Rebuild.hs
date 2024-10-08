{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Lsp.Rebuild (rebuildFile, codegenTargets) where

import Control.Concurrent.STM (TVar)
import Control.Monad.Catch (MonadThrow)
import Data.List qualified as List
import Data.Map.Lazy qualified as M
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.LSP.Server (MonadLsp)
import Language.PureScript.AST qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Errors qualified as P
import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.Ide.Rebuild (updateCacheDb)
import Language.PureScript.Lsp.Cache (selectDependencies)
import Language.PureScript.Lsp.Log (debugLsp, logPerfStandard)
import Language.PureScript.Lsp.ReadFile (lspReadFile)
import Language.PureScript.Lsp.ServerConfig (ServerConfig, getMaxFilesInCache)
import Language.PureScript.Lsp.State (addExternToExportEnv, buildExportEnvCache, cacheRebuild', cachedRebuild)
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig, lspDbConnection, lspStateVar), LspState, OpenFile (OpenFile))
import Language.PureScript.Make (ffiCodegen')
import Language.PureScript.Make qualified as P
import Language.PureScript.Make.Index (addAllIndexing)
import Language.PureScript.Names qualified as P
import Language.PureScript.Options qualified as P
import Protolude hiding (moduleName)

rebuildFile ::
  ( MonadThrow m,
    MonadReader LspEnvironment m,
    MonadLsp ServerConfig m
  ) =>
  FilePath ->
  m (Either ([(FilePath, Text)], P.MultipleErrors) (FilePath, P.MultipleErrors))
rebuildFile srcPath = logPerfStandard ("Rebuild file " <> T.pack srcPath) do
  (fp, input) <-
    case List.stripPrefix "data:" srcPath of
      Just source -> pure ("", T.pack source)
      _ -> lspReadFile srcPath -- todo replace with VFS
  case sequence $ CST.parseFromFile fp input of
    Left parseError ->
      pure $ Left ([(fp, input)], CST.toMultipleErrors fp parseError)
    Right (pwarnings, m) -> do
      let moduleName = P.getModuleName m
      let filePathMap = M.singleton moduleName (Left P.RebuildAlways)
      outputDirectory <- asks (confOutputPath . lspConfig)
      let pureRebuild = fp == ""
      let modulePath = if pureRebuild then fp else srcPath
      conn <- asks lspDbConnection
      stVar <- asks lspStateVar
      maxCache <- getMaxFilesInCache
      cachedBuild <- cachedRebuild srcPath
      let makeEnv foreigns externs =
            P.buildMakeActions outputDirectory filePathMap foreigns False
              & (if pureRebuild then enableForeignCheck foreigns codegenTargets . shushCodegen else identity)
              & shushProgress
              & addAllIndexing conn
              & addRebuildCaching stVar maxCache externs
      debugLsp $ "Cache found: " <> show (isJust cachedBuild)
      case cachedBuild of
        Just (OpenFile _ _ externs env _) -> do
          foreigns <-  P.inferForeignModules (M.singleton moduleName (Right modulePath))
          exportEnv <- logPerfStandard "build export cache" $ buildExportEnvCache m externs
          res <- logPerfStandard "Rebuild Module with provided env" $ liftIO $ do
            P.runMake (P.defaultOptions {P.optionsCodegenTargets = codegenTargets}) do
              newExtern <- P.rebuildModuleWithProvidedEnv (makeEnv foreigns externs) exportEnv env externs m Nothing
              unless pureRebuild $
                updateCacheDb codegenTargets outputDirectory srcPath Nothing moduleName
              pure newExtern
          handleRebuildResult fp input pwarnings res
        Nothing -> do
          externs <- logPerfStandard "Select depenencies" $ selectDependencies m
          foreigns <-  P.inferForeignModules (M.singleton moduleName (Right modulePath))
          exportEnv <- logPerfStandard "build export cache" $ buildExportEnvCache m externs
          res <- logPerfStandard "Rebuild Module" $ liftIO $ do
            P.runMake (P.defaultOptions {P.optionsCodegenTargets = codegenTargets}) do
              newExtern <- P.rebuildModule' (makeEnv foreigns externs) exportEnv externs m
              unless pureRebuild $
                updateCacheDb codegenTargets outputDirectory srcPath Nothing moduleName
              pure newExtern
          handleRebuildResult fp input pwarnings res
  where
    handleRebuildResult fp input pwarnings (result, warnings) = do
      case result of
        Left errors ->
          pure (Left ([(fp, input)], errors))
        Right newExtern -> do
          addExternToExportEnv newExtern
          pure $ Right (fp, CST.toMultipleWarnings fp pwarnings <> warnings)

codegenTargets :: Set P.CodegenTarget
codegenTargets = Set.fromList [P.JS, P.CoreFn, P.Docs]

-- | Shuts the compiler up about progress messages
shushProgress :: (Monad m) => P.MakeActions m -> P.MakeActions m
shushProgress ma =
  ma {P.progress = \_ -> pure ()}

-- | Stops any kind of codegen
shushCodegen :: (Monad m) => P.MakeActions m -> P.MakeActions m
shushCodegen ma =
  ma
    { P.codegen = \_ _ _ _ _ _ -> pure (),
      P.ffiCodegen = \_ -> pure ()
    }

enableForeignCheck ::
  M.Map P.ModuleName FilePath ->
  S.Set P.CodegenTarget ->
  P.MakeActions P.Make ->
  P.MakeActions P.Make
enableForeignCheck foreigns codegenTargets' ma =
  ma
    { P.ffiCodegen = ffiCodegen' foreigns codegenTargets' Nothing
    }

addRebuildCaching :: TVar LspState -> Int -> [ExternsFile] -> P.MakeActions P.Make -> P.MakeActions P.Make
addRebuildCaching stVar maxCache deps ma =
  ma
    { P.codegen = \prevEnv env astM m docs ext -> lift (liftIO $ cacheRebuild' stVar maxCache ext deps prevEnv env) <* P.codegen ma prevEnv env astM m docs ext
    }
