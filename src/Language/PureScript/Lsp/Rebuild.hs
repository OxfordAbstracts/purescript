{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.PureScript.Lsp.Rebuild (rebuildFile, codegenTargets) where

import Control.Monad.Catch (MonadThrow)
import Data.List qualified as List
import Data.Map.Lazy qualified as M
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.PureScript.AST qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Errors qualified as P
import Language.PureScript.Ide.Rebuild (updateCacheDb)
import Language.PureScript.Lsp.Cache (selectDependencies)
import Language.PureScript.Lsp.Log (logPerfStandard)
import Language.PureScript.Lsp.ReadFile (lspReadFile)
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig, lspDbConnection))
import Language.PureScript.Make (ffiCodegen')
import Language.PureScript.Make qualified as P
import Language.PureScript.Make.Index (addAllIndexing)
import Language.PureScript.Names qualified as P
import Language.PureScript.Options qualified as P
import Protolude hiding (moduleName)

rebuildFile ::
  ( MonadIO m,
    MonadThrow m,
    MonadReader LspEnvironment m
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
      externs <- logPerfStandard "Select depenencies" $ selectDependencies m
      outputDirectory <- asks (confOutputPath . lspConfig)
      let filePathMap = M.singleton moduleName (Left P.RebuildAlways)
      let pureRebuild = fp == ""
      let modulePath = if pureRebuild then fp else srcPath
      foreigns <- P.inferForeignModules (M.singleton moduleName (Right modulePath))
      conn <- asks lspDbConnection
      let makeEnv =
            P.buildMakeActions outputDirectory filePathMap foreigns False
              & (if pureRebuild then enableForeignCheck foreigns codegenTargets . shushCodegen else identity)
              & shushProgress
              & addAllIndexing conn
      (!result, warnings) <- logPerfStandard ("Rebuild Module " <> T.pack srcPath) $ fmap force $ liftIO $ do
        P.runMake (P.defaultOptions {P.optionsCodegenTargets = codegenTargets}) do
          newExterns <- P.rebuildModule makeEnv externs m
          unless pureRebuild $
            updateCacheDb codegenTargets outputDirectory srcPath Nothing moduleName
          pure newExterns
      case result of
        Left errors ->
          pure (Left ([(fp, input)], errors))
        Right newExterns -> do
          -- cacheRebuild srcPath newExterns
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
    { P.codegen = \_ _ _ _ _ -> pure (),
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

