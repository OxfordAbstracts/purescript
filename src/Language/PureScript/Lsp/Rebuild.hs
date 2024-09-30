{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.PureScript.Lsp.Rebuild where

import Data.List qualified as List
import Data.Map.Lazy qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.PureScript.AST qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Errors qualified as P
import Language.PureScript.Externs (ExternsFile (efModuleName))
import Language.PureScript.Externs qualified as P
import Language.PureScript.Ide.Error (IdeError (GeneralError, RebuildError))
import Language.PureScript.Ide.Rebuild (updateCacheDb)
import Language.PureScript.Ide.Types (ModuleMap)
import Language.PureScript.Ide.Util (ideReadFile)
import Language.PureScript.Lsp.Cache
import Language.PureScript.Lsp.State (cacheRebuild)
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig, lspDbConnection))
import Language.PureScript.Make (ffiCodegen')
import Language.PureScript.Make qualified as P
import Language.PureScript.ModuleDependencies qualified as P
import Language.PureScript.Names qualified as P
import Language.PureScript.Options qualified as P
import Protolude hiding (moduleName)
import "monad-logger" Control.Monad.Logger (MonadLogger, logDebugN)
import Language.PureScript.Make.Index (addCoreFnIndexing)

rebuildFileAndDeps ::
  ( MonadIO m,
    MonadError IdeError m,
    MonadReader LspEnvironment m,
    MonadLogger m
  ) =>
  FilePath ->
  m (FilePath, P.MultipleErrors)
rebuildFileAndDeps = rebuildFile' True

rebuildFile ::
  ( MonadIO m,
    MonadError IdeError m,
    MonadReader LspEnvironment m,
    MonadLogger m
  ) =>
  FilePath ->
  m (FilePath, P.MultipleErrors)
rebuildFile = rebuildFile' False

rebuildFile' ::
  ( MonadIO m,
    MonadError IdeError m,
    MonadReader LspEnvironment m,
    MonadLogger m
  ) =>
  Bool ->
  FilePath ->
  m (FilePath, P.MultipleErrors)
rebuildFile' rebuildDeps srcPath = do
  logDebugN $ "Rebuilding file: " <> T.pack srcPath
  (fp, input) <-
    case List.stripPrefix "data:" srcPath of
      Just source -> pure ("", T.pack source)
      _ -> ideReadFile srcPath -- todo replace with VFS
  (pwarnings, m) <- case sequence $ CST.parseFromFile fp input of
    Left parseError ->
      throwError $ RebuildError [(fp, input)] $ CST.toMultipleErrors fp parseError
    Right m -> pure m
  let moduleName = P.getModuleName m
  externs <- sortExterns m =<< selectAllExternsMap
  logDebugN $ "Sorted externs: " <> T.pack (show $ map P.efModuleName externs)
  when rebuildDeps do
    forM_ externs \ef -> do
      let depSrcPath = P.spanName $ P.efSourceSpan ef
          modName = P.runModuleName $ P.efModuleName ef
      when (modName /= "Prim" && T.take 5 modName /= "Prim.") do
        logDebugN $ "Rebuilding dependency: " <> T.pack depSrcPath
        void $ rebuildFile' False depSrcPath

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
          & addCoreFnIndexing conn
  (result, warnings) <- liftIO $ P.runMake (P.defaultOptions {P.optionsCodegenTargets = codegenTargets}) do
    (newExterns, coreFn, docs) <- P.rebuildModuleAndGetArtifacts makeEnv externs m
    putErrLn $ "Rebuilt module: " <> T.pack (show coreFn)
    unless pureRebuild $
      updateCacheDb codegenTargets outputDirectory srcPath Nothing moduleName
    pure newExterns
  case result of
    Left errors ->
      throwError (RebuildError [(fp, input)] errors)
    Right newExterns -> do
      insertModule fp m
      insertExtern newExterns
      rebuildModuleOpen makeEnv externs m
      logDebugN $ "Rebuilt file: " <> T.pack srcPath
      pure (fp, CST.toMultipleWarnings fp pwarnings <> warnings)
  where
    codegenTargets = Set.singleton P.JS

-- | Rebuilds a module but opens up its export list first and stores the result
-- inside the rebuild cache
rebuildModuleOpen ::
  ( MonadReader LspEnvironment m,
    MonadIO m
  ) =>
  P.MakeActions P.Make ->
  [P.ExternsFile] ->
  P.Module ->
  m ()
rebuildModuleOpen makeEnv externs m = void $ runExceptT do
  (openResult, _) <-
    liftIO $
      P.runMake P.defaultOptions $
        P.rebuildModule (shushProgress (shushCodegen makeEnv)) externs (openModuleExports m)
  case openResult of
    Left _ ->
      throwError (GeneralError "Failed when rebuilding with open exports")
    Right result -> cacheRebuild result m

-- | Shuts the compiler up about progress messages
shushProgress :: (Monad m) => P.MakeActions m -> P.MakeActions m
shushProgress ma =
  ma {P.progress = \_ -> pure ()}

-- | Stops any kind of codegen
shushCodegen :: (Monad m) => P.MakeActions m -> P.MakeActions m
shushCodegen ma =
  ma
    { P.codegen = \_ _ _ -> pure (),
      P.ffiCodegen = \_ -> pure ()
    }

-- add

enableForeignCheck ::
  M.Map P.ModuleName FilePath ->
  S.Set P.CodegenTarget ->
  P.MakeActions P.Make ->
  P.MakeActions P.Make
enableForeignCheck foreigns codegenTargets ma =
  ma
    { P.ffiCodegen = ffiCodegen' foreigns codegenTargets Nothing
    }

-- | Returns a topologically sorted list of dependent ExternsFiles for the given
-- module. Throws an error if there is a cyclic dependency within the
-- ExternsFiles
sortExterns ::
  (MonadError IdeError m) =>
  P.Module ->
  ModuleMap P.ExternsFile ->
  m [P.ExternsFile]
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
      throwError (RebuildError [] err)
    Right (sorted, graph) -> do
      let deps = fromJust (List.lookup (P.getModuleName m) graph)
      pure $ mapMaybe getExtern (deps `inOrderOf` map P.getModuleName sorted)
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
