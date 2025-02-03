{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.PureScript.Make
  ( -- * Make API
    rebuildModule,
    rebuildModule',
    rebuildModuleWithProvidedEnv,
    make,
    makeDb,
    inferForeignModules,
    module Monad,
    module Actions,
  )
where

import Control.Concurrent.Lifted as C
import Control.DeepSeq (force)
import Control.Exception.Lifted (bracket_, evaluate, onException)
import Control.Monad (foldM, unless, when, (<=<))
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (get)
import Control.Monad.Supply (evalSupplyT, runSupply, runSupplyT)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.State (runStateT)
import Control.Monad.Writer.Class (MonadWriter (..), censor)
import Control.Monad.Writer.Strict (MonadTrans (lift), runWriterT)
import Data.Foldable (fold, for_)
import Data.Function (on)
import Data.List (foldl', intercalate, sortOn)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import Debug.Trace (traceMarkerIO)
import GHC.Conc (enableAllocationLimit, setAllocationCounter)
import Language.PureScript.AST (ErrorMessageHint (..), Module (..), SourceSpan (..), getModuleName, getModuleSourceSpan, importPrim, internalModuleSourceSpan)
import Language.PureScript.CST qualified as CST
import Language.PureScript.CoreFn qualified as CF
import Language.PureScript.Crash (internalError)
import Language.PureScript.Docs.Convert qualified as Docs
import Language.PureScript.Docs.Types qualified as Docs
import Language.PureScript.Environment (Environment, initEnvironment)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage (..), addHint, defaultPPEOptions, errorMessage', errorMessage'', prettyPrintMultipleErrors)
import Language.PureScript.Externs (ExternsFile (..), ExternsFixity, ExternsTypeFixity, applyExternsFileToEnvironment, moduleToExternsFile)
import Language.PureScript.Linter (Name (..), lint, lintImports)
import Language.PureScript.Make.Actions as Actions
import Language.PureScript.Make.BuildPlan (BuildJobResult (..), BuildPlan (..), getResult)
import Language.PureScript.Make.BuildPlan qualified as BuildPlan
import Language.PureScript.Make.BuildPlanDB qualified as BuildPlanDB
import Language.PureScript.Make.Cache qualified as Cache
import Language.PureScript.Make.Index.Select (GetEnv (deleteModuleEnv), dbEnv, getModuleFixities, runDbEnv, runWoGetEnv, selectFixitiesFromModuleImports)
import Language.PureScript.Make.Monad as Monad
import Language.PureScript.ModuleDependencies (DependencyDepth (..), moduleSignature, sortModules)
import Language.PureScript.Names (ModuleName (..), isBuiltinModuleName, runModuleName)
import Language.PureScript.Renamer (renameInModule)
import Language.PureScript.Sugar (Env, collapseBindingGroups, createBindingGroups, desugar, desugarCaseGuards, desugarUsingDb, externsEnv, primEnv)
import Language.PureScript.TypeChecker (CheckState (..), emptyCheckState, typeCheckModule)
import Language.PureScript.TypeChecker.Monad qualified as P
import Protolude (putErrText)
import System.Directory (doesFileExist)
import System.FilePath (replaceExtension)
import Prelude

-- | Rebuild a single module.
--
-- This function is used for fast-rebuild workflows (PSCi and psc-ide are examples).
rebuildModule ::
  forall m.
  (MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  MakeActions m ->
  [ExternsFile] ->
  Module ->
  m ExternsFile
rebuildModule actions externs m = do
  env <- fmap fst . runWriterT $ foldM externsEnv primEnv externs
  rebuildModule' actions env externs m

rebuildModule' ::
  forall m.
  (MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  MakeActions m ->
  Env ->
  [ExternsFile] ->
  Module ->
  m ExternsFile
rebuildModule' act env ext mdl = rebuildModuleWithIndex act env ext mdl Nothing

rebuildModuleWithIndex ::
  forall m.
  (MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  MakeActions m ->
  Env ->
  [ExternsFile] ->
  Module ->
  Maybe (Int, Int) ->
  m ExternsFile
rebuildModuleWithIndex act exEnv externs m moduleIndex = do
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
  rebuildModuleWithProvidedEnv emptyCheckState act exEnv env externs m moduleIndex

rebuildModuleWithIndexDb ::
  forall m.
  (MonadError MultipleErrors m, MonadWriter MultipleErrors m, MonadIO m) =>
  MakeActions m ->
  Connection ->
  Env ->
  Module ->
  Maybe (Int, Int) ->
  m ExternsFile
rebuildModuleWithIndexDb act conn exEnv m moduleIndex = do
  rebuildModuleWithProvidedEnvDb emptyCheckState act conn exEnv m moduleIndex

rebuildModuleWithProvidedEnv ::
  forall m.
  (MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  (Environment -> CheckState) ->
  MakeActions m ->
  Env ->
  Environment ->
  [ExternsFile] ->
  Module ->
  Maybe (Int, Int) ->
  m ExternsFile
rebuildModuleWithProvidedEnv initialCheckState MakeActions {..} exEnv env externs m@(Module _ _ moduleName _ _) moduleIndex = do
  progress $ CompilingModule moduleName moduleIndex
  let withPrim = importPrim m
  lint withPrim
  ((Module ss coms _ elaborated exps, checkSt), nextVar) <-
    desugarAndTypeCheck initialCheckState withCheckStateOnError withCheckState moduleName externs withPrim exEnv env
  let env' = P.checkEnv checkSt

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, nextVar') <- runSupplyT nextVar $ do
    desugarCaseGuards elaborated

  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded
  let mod' = Module ss coms moduleName regrouped exps

  corefn <- runWoGetEnv $ CF.moduleToCoreFn env' mod'
  let (optimized, nextVar'') = runSupply nextVar' $ CF.optimizeCoreFn corefn
      (_renamedIdents, renamed) = renameInModule optimized
  -- exts = moduleToExternsFile mod' env' renamedIdents
  ffiCodegen renamed
  -- It may seem more obvious to write `docs <- Docs.convertModule m env' here,
  -- but I have not done so for two reasons:
  -- 1. This should never fail; any genuine errors in the code should have been
  -- caught earlier in this function. Therefore if we do fail here it indicates
  -- a bug in the compiler, which should be reported as such.
  -- 2. We do not want to perform any extra work generating docs unless the
  -- user has asked for docs to be generated.
  let docs = case Docs.convertModule externs exEnv env' withPrim of
        Left errs ->
          internalError $
            "Failed to produce docs for "
              ++ T.unpack (runModuleName moduleName)
              ++ "; details:\n"
              ++ prettyPrintMultipleErrors defaultPPEOptions errs
        Right d -> d

  evalSupplyT nextVar'' $ codegen env checkSt mod' renamed docs
  return dummyExternsFile

dummyExternsFile :: ExternsFile
dummyExternsFile =
  ExternsFile
    { efVersion = "0",
      efSourceSpan = internalModuleSourceSpan "<dummy>",
      efModuleName = ModuleName "dummy",
      efExports = [],
      efImports = [],
      efFixities = [],
      efTypeFixities = [],
      efDeclarations = []
    }

rebuildModuleWithProvidedEnvDb ::
  forall m.
  (MonadError MultipleErrors m, MonadWriter MultipleErrors m, MonadIO m) =>
  (Environment -> CheckState) ->
  MakeActions m ->
  Connection ->
  Env ->
  Module ->
  Maybe (Int, Int) ->
  m ExternsFile
rebuildModuleWithProvidedEnvDb initialCheckState MakeActions {..} conn exEnv m@(Module _ _ moduleName _ _) moduleIndex = do
  progress $ CompilingModule moduleName moduleIndex
  let withPrim = importPrim m
  lint withPrim
  putErrText $ "linted: " <> T.pack (show moduleName)

  ((Module ss coms _ elaborated exps, checkSt), nextVar) <-
    desugarAndTypeCheckDb initialCheckState conn withCheckStateOnError withCheckState moduleName withPrim exEnv

  putErrText $ "type checked: " <> T.pack (show moduleName)

  let env' = P.checkEnv checkSt

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, nextVar') <- runSupplyT nextVar $ do
    desugarCaseGuards elaborated

  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded
  putErrText $ "regrouped: " <> T.pack (show moduleName)
  let mod' = Module ss coms moduleName regrouped exps

  !corefn <- fmap force $ runDbEnv conn $ CF.moduleToCoreFn env' mod'
  putErrText $ "corefn: " <> T.pack (show moduleName)
  let -- !(optimized, nextVar'') = force $ runSupply nextVar' $ CF.optimizeCoreFn corefn
      optimized = corefn
      nextVar'' = nextVar'
  putErrText $ "optimized: " <> T.pack (show moduleName)

  let !(_renamedIdents, renamed) = force (renameInModule optimized)
  putErrText $ "renamed: " <> T.pack (show moduleName)

  -- exts = moduleToExternsFile mod' env' renamedIdents
  ffiCodegen renamed
  putErrText $ "ffi codegen: " <> T.pack (show moduleName)
  -- It may seem more obvious to write `docs <- Docs.convertModule m env' here,
  -- but I have not done so for two reasons:
  -- 1. This should never fail; any genuine errors in the code should have been
  -- caught earlier in this function. Therefore if we do fail here it indicates
  -- a bug in the compiler, which should be reported as such.
  -- 2. We do not want to perform any extra work generating docs unless the
  -- user has asked for docs to be generated.
  let docs = Docs.Module moduleName (Just "TODO") [] []
  -- case Docs.convertModuleWithoutExterns ops typeOps exEnv env' withPrim of
  --     Left errs ->
  --       internalError $
  --         "Failed to produce docs for "
  --           ++ T.unpack (runModuleName moduleName)
  --           ++ "; details:\n"
  --           ++ prettyPrintMultipleErrors defaultPPEOptions errs
  --     Right d -> d
  evalSupplyT nextVar'' $ codegen env' checkSt mod' renamed docs
  putErrText $ "codegen done: " <> T.pack (show moduleName)
  return dummyExternsFile

desugarAndTypeCheck ::
  forall m.
  (MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  (Environment -> CheckState) ->
  (CheckState -> m ()) ->
  (CheckState -> m ()) ->
  ModuleName ->
  [ExternsFile] ->
  Module ->
  Env ->
  Environment ->
  m ((Module, CheckState), Integer)
desugarAndTypeCheck initialCheckState withCheckStateOnError withCheckState moduleName externs withPrim exEnv env = runSupplyT 0 $ do
  (desugared, (exEnv', usedImports)) <- runStateT (desugar externs withPrim) (exEnv, mempty)
  let modulesExports = (\(_, _, exports) -> exports) <$> exEnv'
  (checked, checkSt@(CheckState {..})) <- runStateT (catchError (runWoGetEnv $ typeCheckModule modulesExports desugared) mergeCheckState) $ initialCheckState env
  lift $ withCheckState checkSt
  let usedImports' =
        foldl'
          ( flip $ \(fromModuleName, newtypeCtorName) ->
              M.alter (Just . (fmap DctorName newtypeCtorName :) . fold) fromModuleName
          )
          usedImports
          checkConstructorImportsForCoercible
  -- Imports cannot be linted before type checking because we need to
  -- known which newtype constructors are used to solve Coercible
  -- constraints in order to not report them as unused.
  censor (addHint (ErrorInModule moduleName)) $ lintImports checked exEnv' usedImports'
  return (checked, checkSt)
  where
    mergeCheckState errs = do
      checkSt <- get
      lift $ lift $ withCheckStateOnError checkSt
      throwError errs

desugarAndTypeCheckDb ::
  forall m.
  (MonadError MultipleErrors m, MonadIO m, MonadWriter MultipleErrors m) =>
  (Environment -> CheckState) ->
  Connection ->
  (CheckState -> m ()) ->
  (CheckState -> m ()) ->
  ModuleName ->
  Module ->
  Env ->
  m ((Module, CheckState), Integer)
desugarAndTypeCheckDb initialCheckState conn withCheckStateOnError _withCheckState moduleName withPrim exEnv = runSupplyT 0 $ do
  runDbEnv conn $ deleteModuleEnv moduleName
  (desugared, (exEnv', usedImports)) <- runStateT (desugarUsingDb conn exEnv withPrim) (exEnv, mempty)
  let modulesExports = (\(_, _, exports) -> exports) <$> exEnv'
  -- env <- selectEnvFromDefinitions conn exEnv' desugared
  let env = initEnvironment
  (checked, checkSt@(CheckState {..})) <- runStateT (catchError (runDbEnv conn $ typeCheckModule modulesExports desugared) mergeCheckState) (initialCheckState env)
  let usedImports' =
        foldl'
          ( flip $ \(fromModuleName, newtypeCtorName) ->
              M.alter (Just . (fmap DctorName newtypeCtorName :) . fold) fromModuleName
          )
          usedImports
          checkConstructorImportsForCoercible
  -- Imports cannot be linted before type checking because we need to
  -- known which newtype constructors are used to solve Coercible
  -- constraints in order to not report them as unused.
  censor (addHint (ErrorInModule moduleName)) $ lintImports checked exEnv' usedImports'
  return (checked, checkSt)
  where
    mergeCheckState errs = do
      checkSt <- get
      lift $ lift $ withCheckStateOnError checkSt
      throwError errs

-- | Compiles in "make" mode, compiling each module separately to a @.js@ file and an @externs.cbor@ file.
--
-- If timestamps or hashes have not changed, existing externs files can be used to provide upstream modules' types without
-- having to typecheck those modules again.
make ::
  forall m.
  (MonadBaseControl IO m, MonadIO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  MakeActions m ->
  [CST.PartialResult Module] ->
  m [ExternsFile]
make ma@MakeActions {..} ms = do
  checkModuleNames
  cacheDb <- readCacheDb
  conn <- getDbConnection

  (sorted, graph) <- sortModules Transitive (moduleSignature . CST.resPartial) ms

  (buildPlan, newCacheDb) <- BuildPlan.construct ma cacheDb (sorted, graph)

  -- Limit concurrent module builds to the number of capabilities as
  -- (by default) inferred from `+RTS -N -RTS` or set explicitly like `-N4`.
  -- This is to ensure that modules complete fully before moving on, to avoid
  -- holding excess memory during compilation from modules that were paused
  -- by the Haskell runtime.
  capabilities <- getNumCapabilities
  let concurrency = max 1 capabilities
  lock <- C.newQSem concurrency

  let toBeRebuilt = filter (BuildPlan.needsRebuild buildPlan . getModuleName . CST.resPartial) sorted
  let totalModuleCount = length toBeRebuilt
  for_ toBeRebuilt $ \m -> fork $ do
    let moduleName = getModuleName . CST.resPartial $ m
    let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
    buildModule
      conn
      lock
      buildPlan
      moduleName
      totalModuleCount
      (spanName . getModuleSourceSpan . CST.resPartial $ m)
      (fst $ CST.resFull m)
      (fmap importPrim . snd $ CST.resFull m)
      (deps `inOrderOf` map (getModuleName . CST.resPartial) sorted)
      -- Prevent hanging on other modules when there is an internal error
      -- (the exception is thrown, but other threads waiting on MVars are released)
      `onException` BuildPlan.markComplete buildPlan moduleName (BuildJobFailed mempty)

  -- Wait for all threads to complete, and collect results (and errors).
  (failures, successes) <-
    let splitResults = \case
          BuildJobSucceeded _ exts ->
            Right exts
          BuildJobFailed errs ->
            Left errs
          BuildJobSkipped ->
            Left mempty
     in M.mapEither splitResults <$> BuildPlan.collectResults buildPlan

  -- Write the updated build cache database to disk
  writeCacheDb $ Cache.removeModules (M.keysSet failures) newCacheDb

  writePackageJson

  -- If generating docs, also generate them for the Prim modules
  outputPrimDocs

  -- All threads have completed, rethrow any caught errors.
  let errors = M.elems failures
  unless (null errors) $ throwError (mconcat errors)

  -- Here we return all the ExternsFile in the ordering of the topological sort,
  -- so they can be folded into an Environment. This result is used in the tests
  -- and in PSCI.
  let lookupResult mn =
        fromMaybe (internalError "make: module not found in results") $
          M.lookup mn successes
  return (map (lookupResult . getModuleName . CST.resPartial) sorted)
  where
    checkModuleNames :: m ()
    checkModuleNames = checkNoPrim *> checkModuleNamesAreUnique

    checkNoPrim :: m ()
    checkNoPrim =
      for_ ms $ \m ->
        let mn = getModuleName $ CST.resPartial m
         in when (isBuiltinModuleName mn)
              $ throwError
                . errorMessage' (getModuleSourceSpan $ CST.resPartial m)
              $ CannotDefinePrimModules mn

    checkModuleNamesAreUnique :: m ()
    checkModuleNamesAreUnique =
      for_ (findDuplicates (getModuleName . CST.resPartial) ms) $ \mss ->
        throwError . flip foldMap mss $ \ms' ->
          let mn = getModuleName . CST.resPartial . NEL.head $ ms'
           in errorMessage'' (fmap (getModuleSourceSpan . CST.resPartial) ms') $ DuplicateModule mn

    -- Find all groups of duplicate values in a list based on a projection.
    findDuplicates :: (Ord b) => (a -> b) -> [a] -> Maybe [NEL.NonEmpty a]
    findDuplicates f xs =
      case filter ((> 1) . length) . NEL.groupBy ((==) `on` f) . sortOn f $ xs of
        [] -> Nothing
        xss -> Just xss

    -- Sort a list so its elements appear in the same order as in another list.
    inOrderOf :: (Ord a) => [a] -> [a] -> [a]
    inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

    buildModule :: Connection -> QSem -> BuildPlan -> ModuleName -> Int -> FilePath -> [CST.ParserWarning] -> Either (NEL.NonEmpty CST.ParserError) Module -> [ModuleName] -> m ()
    buildModule conn lock buildPlan moduleName cnt fp pwarnings mres deps = do
      result <- flip catchError (return . BuildJobFailed) $ do
        let pwarnings' = CST.toMultipleWarnings fp pwarnings
        tell pwarnings'
        m <- CST.unwrapParserError fp mres
        -- We need to wait for dependencies to be built, before checking if the current
        -- module should be rebuilt, so the first thing to do is to wait on the
        -- MVars for the module's dependencies.
        mexterns <- fmap unzip . sequence <$> traverse (getResult buildPlan) deps

        case mexterns of
          Just (_, externs) -> do
            -- We need to ensure that all dependencies have been included in Env
            C.modifyMVar_ (bpEnv buildPlan) $ \env -> do
              let go :: Env -> ModuleName -> m Env
                  go e dep = case lookup dep (zip deps externs) of
                    Just exts
                      | not (M.member dep e) -> externsEnv e exts
                    _ -> return e
              foldM go env deps
            env <- C.readMVar (bpEnv buildPlan)
            idx <- C.takeMVar (bpIndex buildPlan)
            C.putMVar (bpIndex buildPlan) (idx + 1)

            -- Bracket all of the per-module work behind the semaphore, including
            -- forcing the result. This is done to limit concurrency and keep
            -- memory usage down; see comments above.
            (exts, warnings) <- bracket_ (C.waitQSem lock) (C.signalQSem lock) $ do
              -- Eventlog markers for profiling; see debug/eventlog.js
              liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " start"
              -- Force the externs and warnings to avoid retaining excess module
              -- data after the module is finished compiling.
              extsAndWarnings <- evaluate . force <=< listen $ do
                rebuildModuleWithIndexDb ma conn env m (Just (idx, cnt))
              liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " end"
              return extsAndWarnings
            return $ BuildJobSucceeded (pwarnings' <> warnings) exts
          Nothing -> return BuildJobSkipped

      BuildPlan.markComplete buildPlan moduleName result

makeDb ::
  forall m.
  (MonadBaseControl IO m, MonadIO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  MakeActions m ->
  [CST.PartialResult Module] ->
  m [ModuleName]
makeDb ma@MakeActions {..} ms = do
  checkModuleNames
  cacheDb <- readCacheDb
  conn <- getDbConnection

  (sorted, graph) <- sortModules Transitive (moduleSignature . CST.resPartial) ms

  (buildPlan, _newCacheDb) <- BuildPlanDB.construct ma cacheDb (sorted, graph)

  -- Limit concurrent module builds to the number of capabilities as
  -- (by default) inferred from `+RTS -N -RTS` or set explicitly like `-N4`.
  -- This is to ensure that modules complete fully before moving on, to avoid
  -- holding excess memory during compilation from modules that were paused
  -- by the Haskell runtime.
  capabilities <- getNumCapabilities
  let concurrency = max 1 capabilities
  putErrText $ "concurrency: " <> T.pack (show concurrency)
  lock <- C.newQSem concurrency

  let toBeRebuilt = filter (BuildPlanDB.needsRebuild buildPlan . getModuleName . CST.resPartial) sorted
  let totalModuleCount = length toBeRebuilt

      -- importedModules :: S.Set ModuleName
      -- importedModules = S.fromList $ graph >>= snd

      -- orphan :: ModuleName -> Bool
      -- orphan mn = S.notMember mn importedModules

      graphMap :: M.Map ModuleName [ModuleName]
      graphMap = M.fromList graph

  for_ toBeRebuilt $ \m -> fork $ do
    liftIO do
      setAllocationCounter 8.0e9
      enableAllocationLimit
    let moduleName = getModuleName . CST.resPartial $ m
    let deps = fromMaybe (internalError "make: module not found in dependency graph.") (M.lookup moduleName graphMap)

    let buildModule' =
          buildModule
            conn
            lock
            buildPlan
            moduleName
            totalModuleCount
            (getModuleSourceSpan . CST.resPartial $ m)
            (fst $ CST.resFull m)
            (fmap importPrim . snd $ CST.resFull m)
            (deps `inOrderOf` map (getModuleName . CST.resPartial) sorted)
            -- Prevent hanging on other modules when there is an internal error
            -- (the exception is thrown, but other threads waiting on MVars are released)
            `onException` do
              putErrText $ "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Exception building: " <> runModuleName moduleName
              BuildPlanDB.markComplete buildPlan moduleName (BuildPlanDB.BuildJobFailed mempty)

    -- if orphan moduleName
    --   then BuildPlanDB.markComplete buildPlan moduleName (BuildPlanDB.BuildJobSucceeded mempty)
    --   else
    buildModule'

  -- Wait for all threads to complete, and collect results (and errors).
  (failures, _successes) <-
    let splitResults = \case
          BuildPlanDB.BuildJobSucceeded _ ->
            Right ()
          BuildPlanDB.BuildJobFailed errs ->
            Left errs
          BuildPlanDB.BuildJobSkipped ->
            Left mempty
     in M.mapEither splitResults <$> BuildPlanDB.collectResults buildPlan

  writePackageJson

  -- If generating docs, also generate them for the Prim modules
  outputPrimDocs

  -- All threads have completed, rethrow any caught errors.
  let errors = M.elems failures
  unless (null errors) $ throwError (mconcat errors)

  return (map (getModuleName . CST.resPartial) sorted)
  where
    checkModuleNames :: m ()
    checkModuleNames = checkNoPrim *> checkModuleNamesAreUnique

    checkNoPrim :: m ()
    checkNoPrim =
      for_ ms $ \m ->
        let mn = getModuleName $ CST.resPartial m
         in when (isBuiltinModuleName mn)
              $ throwError
                . errorMessage' (getModuleSourceSpan $ CST.resPartial m)
              $ CannotDefinePrimModules mn

    checkModuleNamesAreUnique :: m ()
    checkModuleNamesAreUnique =
      for_ (findDuplicates (getModuleName . CST.resPartial) ms) $ \mss ->
        throwError . flip foldMap mss $ \ms' ->
          let mn = getModuleName . CST.resPartial . NEL.head $ ms'
           in errorMessage'' (fmap (getModuleSourceSpan . CST.resPartial) ms') $ DuplicateModule mn

    -- Find all groups of duplicate values in a list based on a projection.
    findDuplicates :: (Ord b) => (a -> b) -> [a] -> Maybe [NEL.NonEmpty a]
    findDuplicates f xs =
      case filter ((> 1) . length) . NEL.groupBy ((==) `on` f) . sortOn f $ xs of
        [] -> Nothing
        xss -> Just xss

    -- Sort a list so its elements appear in the same order as in another list.
    inOrderOf :: (Ord a) => [a] -> [a] -> [a]
    inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

    buildModule :: Connection -> QSem -> BuildPlanDB.BuildPlan -> ModuleName -> Int -> SourceSpan -> [CST.ParserWarning] -> Either (NEL.NonEmpty CST.ParserError) Module -> [ModuleName] -> m ()
    buildModule conn lock buildPlan moduleName cnt ss pwarnings mres deps = do
      let fp = spanName ss
      result <- flip catchError (return . BuildPlanDB.BuildJobFailed) $ do
        let pwarnings' = CST.toMultipleWarnings fp pwarnings
        tell pwarnings'
        m <- CST.unwrapParserError fp mres
        -- We need to wait for dependencies to be built, before checking if the current
        -- module should be rebuilt, so the first thing to do is to wait on the
        -- MVars for the module's dependencies.
        mexterns <- sequence <$> traverse (BuildPlanDB.getResult buildPlan) deps
        -- let lookupResult mn =
        --       fromMaybe (internalError "make: module not found in results") $
        --         M.lookup mn _

        case mexterns of
          Just externs -> do
            -- We need to ensure that all dependencies have been included in Env
            C.modifyMVar_ (BuildPlanDB.bpEnv buildPlan) $ \env -> do
              let go :: Env -> ModuleName -> m Env
                  go e dep = case lookup dep (zip deps externs) of
                    Just _exts
                      | not (M.member dep e) -> dbEnv conn e ss dep
                    _ -> return e
              foldM go env deps
            env <- C.readMVar (BuildPlanDB.bpEnv buildPlan)
            idx <- C.takeMVar (BuildPlanDB.bpIndex buildPlan)
            C.putMVar (BuildPlanDB.bpIndex buildPlan) (idx + 1)

            -- Bracket all of the per-module work behind the semaphore, including
            -- forcing the result. This is done to limit concurrency and keep
            -- memory usage down; see comments above.
            (_e, warnings) <- bracket_ (C.waitQSem lock) (C.signalQSem lock) $ do
              -- Eventlog markers for profiling; see debug/eventlog.js
              liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " start"
              -- Force the externs and warnings to avoid retaining excess module
              -- data after the module is finished compiling.
              extsAndWarnings <- evaluate . force <=< listen $ do
                rebuildModuleWithIndexDb ma conn env m (Just (idx, cnt))
              liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " end"
              return extsAndWarnings
            return $ BuildPlanDB.BuildJobSucceeded (pwarnings' <> warnings)
          Nothing -> return BuildPlanDB.BuildJobSkipped

      BuildPlanDB.markComplete buildPlan moduleName result

-- | Infer the module name for a module by looking for the same filename with
-- a .js extension.
inferForeignModules ::
  forall m.
  (MonadIO m) =>
  M.Map ModuleName (Either RebuildPolicy FilePath) ->
  m (M.Map ModuleName FilePath)
inferForeignModules =
  fmap (M.mapMaybe id) . traverse inferForeignModule
  where
    inferForeignModule :: Either RebuildPolicy FilePath -> m (Maybe FilePath)
    inferForeignModule (Left _) = return Nothing
    inferForeignModule (Right path) = do
      let jsFile = replaceExtension path "js"
      exists <- liftIO $ doesFileExist jsFile
      if exists
        then return (Just jsFile)
        else return Nothing
