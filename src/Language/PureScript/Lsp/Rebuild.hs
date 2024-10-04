{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.PureScript.Lsp.Rebuild where

import Control.Monad.Catch (MonadThrow)
import Data.List qualified as List
import Data.Map.Lazy qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.PureScript (MultipleErrors)
import Language.PureScript.AST qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Errors qualified as P
import Language.PureScript.Externs (ExternsFile (efModuleName))
import Language.PureScript.Externs qualified as P
import Language.PureScript.Ide.Rebuild (updateCacheDb)
import Language.PureScript.Ide.Types (ModuleMap)
import Language.PureScript.Lsp.Cache (selectAllExternsMap)
import Language.PureScript.Lsp.ReadFile (lspReadFile)
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig, lspDbConnection))
import Language.PureScript.Make (ffiCodegen')
import Language.PureScript.Make qualified as P
import Language.PureScript.Make.Index (addAllIndexing)
import Language.PureScript.ModuleDependencies qualified as P
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
rebuildFile srcPath = do
  (fp, input) <-
    case List.stripPrefix "data:" srcPath of
      Just source -> pure ("", T.pack source)
      _ -> lspReadFile srcPath -- todo replace with VFS
  case sequence $ CST.parseFromFile fp input of
    Left parseError ->
      pure $ Left ([(fp, input)], CST.toMultipleErrors fp parseError)
    Right (pwarnings, m) -> do
      let moduleName = P.getModuleName m
      externsResult <- sortExterns m =<< selectAllExternsMap
      case externsResult of
        Left err -> pure $ Left $ ([], err)
        Right externs -> do
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
          (result, warnings) <- liftIO $ P.runMake (P.defaultOptions {P.optionsCodegenTargets = codegenTargets}) do
            newExterns <- P.rebuildModule makeEnv externs m
            unless pureRebuild $
              updateCacheDb codegenTargets outputDirectory srcPath Nothing moduleName
            pure newExterns
          case result of
            Left errors ->
              pure (Left ([(fp, input)], errors))
            Right newExterns -> do
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

-- add

enableForeignCheck ::
  M.Map P.ModuleName FilePath ->
  S.Set P.CodegenTarget ->
  P.MakeActions P.Make ->
  P.MakeActions P.Make
enableForeignCheck foreigns codegenTargets' ma =
  ma
    { P.ffiCodegen = ffiCodegen' foreigns codegenTargets' Nothing
    }

-- | Returns a topologically sorted list of dependent ExternsFiles for the given
-- module. Throws an error if there is a cyclic dependency within the
-- ExternsFiles
sortExterns ::
  (MonadThrow m) =>
  P.Module ->
  ModuleMap P.ExternsFile ->
  m (Either MultipleErrors [P.ExternsFile])
sortExterns m ex = do
  sorted' <-
    runExceptT
      . P.sortModules P.Transitive P.moduleSignature
      . (:) m
      . map mkShallowModule
      . M.elems
      . M.delete (P.getModuleName m)
      $ ex
  case sorted' of
    Left err ->
      pure $ Left err
    Right (sorted, graph) -> do
      let deps = fromJust (List.lookup (P.getModuleName m) graph)
      pure $ Right $ mapMaybe getExtern (deps `inOrderOf` map P.getModuleName sorted)
  where
    mkShallowModule P.ExternsFile {..} =
      P.Module (P.internalModuleSourceSpan "<rebuild>") [] efModuleName (map mkImport efImports) Nothing
    mkImport (P.ExternsImport mn it iq) =
      P.ImportDeclaration (P.internalModuleSourceSpan "<rebuild>", []) mn it iq
    getExtern mn = M.lookup mn ex
    -- Sort a list so its elements appear in the same order as in another list.
    inOrderOf :: (Ord a) => [a] -> [a] -> [a]
    inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

-- | Removes a modules export list.
openModuleExports :: P.Module -> P.Module
openModuleExports (P.Module ss cs mn decls _) = P.Module ss cs mn decls Nothing
