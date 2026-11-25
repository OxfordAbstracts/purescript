module Language.PureScript.Make
  ( make
  , make_
  , rebuildModule
  , rebuildModule'
  , inferForeignModules
  , module Monad
  , module Actions
  ) where

import Prelude

import Control.Concurrent.Async.Chunked (forConcurrentlyChunked)
import Control.Concurrent.Lifted as C
import Control.DeepSeq (force, rnf)
import Control.Exception.Lifted (IOException, onException, bracket_, evaluate, finally, handle, uninterruptibleMask_)
import Control.Monad (foldM, unless, void, when, (<=<))
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Error.Class (MonadError(..), liftEither)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Supply (evalSupplyT, runSupply, runSupplyT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.State (runStateT)
import Control.Monad.Writer.Class (MonadWriter(..), censor)
import Control.Monad.Writer.Strict (runWriterT)
import Control.Parallel.Strategies (parListChunk, rseq, withStrategy)
import Data.Array (Array, accumArray, bounds, listArray, range, (!))
import Data.Function (on)
import Data.Foldable (fold, for_, traverse_)
import Data.Functor (($>))
import Data.List (foldl', sortOn)
import Data.IntSet qualified as IS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List.NonEmpty qualified as NEL
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (traceMarkerIO)
import Language.PureScript.AST (ErrorMessageHint(..), Module(..), SourceSpan(..), getModuleName, getModuleSourceSpan, importPrim)
import Language.PureScript.Crash (internalError)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Docs.Convert qualified as Docs
import Language.PureScript.Environment (Environment(..), initEnvironment)
import Language.PureScript.Errors (MultipleErrors(..), SimpleErrorMessage(..), addHint, defaultPPEOptions, errorMessage', errorMessage'', prettyPrintMultipleErrors)
import Language.PureScript.Externs (ExternsFile, applyExternsFileToEnvironment, moduleToExternsFile)
import Language.PureScript.Linter (Name(..), lint, lintImports)
import Language.PureScript.ModuleDependencies (DependencyDepth(..), moduleSignature, sortModules)
import Language.PureScript.Names (ModuleName(..), isBuiltinModuleName, runModuleName)
import Language.PureScript.Renamer (renameInModule)
import Language.PureScript.Sugar (Env, collapseBindingGroups, createBindingGroups, desugar, desugarCaseGuards, externsEnv, primEnv)
import Language.PureScript.TypeChecker (CheckState(..), emptyCheckState, typeCheckModule)
import Language.PureScript.Make.BuildPlan (BuildJobResult(..), BuildPlan(..), getResult, isUpToDate)
import Language.PureScript.Make.BuildPlan qualified as BuildPlan
import Language.PureScript.Make.ExternsDiff (checkDiffs, emptyDiff, diffExterns, isEmptyDiff)
import Language.PureScript.Make.Cache qualified as Cache
import Language.PureScript.Make.Actions as Actions
import Language.PureScript.Make.Monad as Monad
    ( Make(..),
      writeTextFile,
      writeJSONFile,
      writeCborFileIO,
      writeCborFile,
      setTimestamp,
      runMake,
      readTextFile,
      readJSONFileIO,
      readJSONFile,
      readExternsFile,
      readCborFileIO,
      readCborFile,
      makeIO,
      hashFile,
      getTimestampMaybe,
      getTimestamp,
      getCurrentTime,
      copyFile )
import Language.PureScript.CoreFn qualified as CF
import System.Directory (doesFileExist, getFileSize)
import System.FilePath (replaceExtension)
import Language.PureScript.TypeChecker.Monad (liftTypeCheckM)

-- | Rebuild a single module.
--
rebuildModule
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule actions externs m = do
  env <- fmap fst . runWriterT $ foldM externsEnv primEnv externs
  rebuildModule' actions env externs m

rebuildModule'
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> Env
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule' act env ext mdl = rebuildModuleWithIndex act env checkEnv Nothing ext mdl Nothing
  where
  checkEnv = foldl' (flip applyExternsFileToEnvironment) initEnvironment ext

rebuildModuleWithIndex
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> Env
  -> Environment
  -> Maybe (S.Set ModuleName)
  -> [ExternsFile]
  -> Module
  -> Maybe (Int, Int)
  -> m ExternsFile
rebuildModuleWithIndex MakeActions{..} exEnv env visibleModules externs m@(Module _ _ moduleName _ _) moduleIndex = do
  progress $ CompilingModule moduleName moduleIndex
  let withPrim = importPrim m
  lint withPrim

  ((Module ss coms _ elaborated exps, env'), nextVar) <- runSupplyT 0 $ do
    (desugared, (exEnv', usedImports)) <- runStateT (desugar externs withPrim) (exEnv, mempty)
    let modulesExports = (\(_, _, exports) -> exports) <$> exEnv'
    (checked, CheckState{..}) <- runStateT (liftTypeCheckM $ typeCheckModule modulesExports desugared) $ (emptyCheckState env) { checkVisibleModules = visibleModules }
    let usedImports' = foldl' (flip $ \(fromModuleName, newtypeCtorName) ->
          M.alter (Just . (fmap DctorName newtypeCtorName :) . fold) fromModuleName) usedImports checkConstructorImportsForCoercible
    -- Imports cannot be linted before type checking because we need to
    -- known which newtype constructors are used to solve Coercible
    -- constraints in order to not report them as unused.
    censor (addHint (ErrorInModule moduleName)) $ lintImports checked exEnv' usedImports'
    return (checked, checkEnv)

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, nextVar') <- liftEither . runSupplyT nextVar $ desugarCaseGuards elaborated

  regrouped <- liftEither . createBindingGroups moduleName . collapseBindingGroups $ deguarded
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      (optimized, nextVar'') = runSupply nextVar' $ CF.optimizeCoreFn corefn
      (renamedIdents, renamed) = renameInModule optimized
      exts = moduleToExternsFile mod' env' renamedIdents
  ffiCodegen renamed

  -- It may seem more obvious to write `docs <- Docs.convertModule m env' here,
  -- but I have not done so for two reasons:
  -- 1. This should never fail; any genuine errors in the code should have been
  -- caught earlier in this function. Therefore if we do fail here it indicates
  -- a bug in the compiler, which should be reported as such.
  -- 2. We do not want to perform any extra work generating docs unless the
  -- user has asked for docs to be generated.
  let docs = case Docs.convertModule externs exEnv env' withPrim of
               Left errs -> internalError $
                 "Failed to produce docs for " ++ T.unpack (runModuleName moduleName)
                 ++ "; details:\n" ++ prettyPrintMultipleErrors defaultPPEOptions errs
               Right d -> d

  evalSupplyT nextVar'' $ codegen withPrim renamed docs exts
  return exts

data MakeOptions = MakeOptions
  { moCollectAllExterns :: Bool
  }

newtype BuildSlots = BuildSlots (C.MVar (Int, M.Map (Integer, Int) (C.MVar ())))

newBuildSlots :: MonadBase IO m => Int -> m BuildSlots
newBuildSlots count = BuildSlots <$> C.newMVar (count, M.empty)

takeBuildSlot :: MonadBase IO m => BuildSlots -> (Integer, Int) -> m ()
takeBuildSlot slots@(BuildSlots state) (weight, rank) = liftBase $ do
  turn <- C.newEmptyMVar
  queued <- C.modifyMVar state $ \(free, waiting) -> pure $
    if free > 0
      then ((free - 1, waiting), False)
      else ((free, M.insert key turn waiting), True)
  when queued $ C.takeMVar turn `onException` uninterruptibleMask_ abandon
  where
  key = (negate weight, rank)
  abandon = do
    handedOver <- C.modifyMVar state $ \(free, waiting) ->
      pure ((free, M.delete key waiting), M.notMember key waiting)
    when handedOver $ releaseBuildSlot slots

releaseBuildSlot :: MonadBase IO m => BuildSlots -> m ()
releaseBuildSlot (BuildSlots state) = liftBase $ C.modifyMVar_ state $ \(free, waiting) ->
  case M.minView waiting of
    Nothing -> pure (free + 1, waiting)
    Just (turn, rest) -> C.putMVar turn () $> (free, rest)

sourceSizeOrZero :: FilePath -> IO Integer
sourceSizeOrZero = handle (\(_ :: IOException) -> pure 0) . getFileSize

data ProgressQueue = ProgressQueue
  { pqPending :: IORef [ProgressMessage]
  , pqWriter :: C.MVar ()
  }

newProgressQueue :: MonadBase IO m => m ProgressQueue
newProgressQueue = ProgressQueue <$> liftBase (newIORef []) <*> C.newMVar ()

enqueueProgress :: MonadBaseControl IO m => (ProgressMessage -> m ()) -> ProgressQueue -> ProgressMessage -> m ()
enqueueProgress write queue msg = do
  liftBase $ atomicModifyIORef' (pqPending queue) (\msgs -> (msg : msgs, ()))
  writePendingProgress write queue

writePendingProgress :: MonadBaseControl IO m => (ProgressMessage -> m ()) -> ProgressQueue -> m ()
writePendingProgress write queue@ProgressQueue{..} = do
  becameWriter <- C.tryTakeMVar pqWriter
  for_ becameWriter $ \() -> do
    drainProgress write queue `finally` C.putMVar pqWriter ()
    stillPending <- liftBase $ readIORef pqPending
    unless (null stillPending) $ writePendingProgress write queue

drainProgress :: MonadBase IO m => (ProgressMessage -> m ()) -> ProgressQueue -> m ()
drainProgress write ProgressQueue{..} =
  liftBase (atomicModifyIORef' pqPending (\msgs -> ([], msgs))) >>= traverse_ write . reverse

flushProgress :: MonadBaseControl IO m => (ProgressMessage -> m ()) -> ProgressQueue -> m ()
flushProgress write queue@ProgressQueue{..} =
  bracket_ (C.takeMVar pqWriter) (C.putMVar pqWriter ()) (drainProgress write queue)

-- | Compiles in "make" mode, compiling each module separately to a @.js@ file
-- and an @externs.cbor@ file.
--
-- If timestamps or hashes have not changed, existing externs files can be used
-- to provide upstream modules' types without having to typecheck those modules
-- again.
--
-- It collects and returns externs for all modules passed.
make :: forall m. (MonadIO m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeActions m
     -> [CST.PartialResult Module]
     -> m [ExternsFile]
make  = make' (MakeOptions {moCollectAllExterns = True})

-- | Compiles in "make" mode, compiling each module separately to a @.js@ file
-- and an @externs.cbor@ file.
--
-- This version of make returns nothing.
make_ :: forall m. (MonadIO m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeActions m
     -> [CST.PartialResult Module]
     -> m ()
make_ ma ms = void $ make' (MakeOptions {moCollectAllExterns = False}) ma ms

make' :: forall m. (MonadIO m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeOptions
     -> MakeActions m
     -> [CST.PartialResult Module]
     -> m [ExternsFile]
make' MakeOptions{..} ma@MakeActions{..} ms = do
  checkModuleNames
  cacheDb <- readCacheDb

  (sorted, graph) <- sortModules Direct (moduleSignature . CST.resPartial) ms
  let opts = BuildPlan.Options {optPreloadAllExterns = moCollectAllExterns}
  (buildPlan, newCacheDb) <- BuildPlan.construct opts ma cacheDb (sorted, graph)

  -- Limit concurrent module builds to the number of capabilities as
  -- (by default) inferred from `+RTS -N -RTS` or set explicitly like `-N4`.
  -- This is to ensure that modules complete fully before moving on, to avoid
  -- holding excess memory during compilation from modules that were paused
  -- by the Haskell runtime.
  capabilities <- getNumCapabilities
  let concurrency = max 1 capabilities
  slots <- newBuildSlots concurrency
  progressQueue <- newProgressQueue
  let reportProgress = enqueueProgress progress progressQueue

  sortedModuleNames <- liftBase . evaluate . force $ getModuleName . CST.resPartial <$> sorted
  let moduleRanks = M.fromList (zip sortedModuleNames [0 :: Int ..])
  let rankBounds = (0, M.size moduleRanks - 1)
  let modulesByRank = listArray rankBounds sortedModuleNames :: Array Int ModuleName
  let resultsByRank = BuildPlan.resultSource buildPlan <$> modulesByRank
  let graphMap = M.fromList graph
  let directDepRanks moduleName =
        map (moduleRanks M.!) $
          fromMaybe (internalError "make: module not found in dependency graph.") (M.lookup moduleName graphMap)
  let transitiveDepRanks :: Array Int IS.IntSet
      transitiveDepRanks = listArray rankBounds
        [ IS.unions [ IS.insert dep (transitiveDepRanks ! dep) | dep <- directDepRanks moduleName ]
        | moduleName <- sortedModuleNames
        ]
  let toBeRebuilt = filter (BuildPlan.needsRebuild buildPlan . getModuleName . CST.resPartial . snd) (zip [0 ..] sorted)
  _ <- liftBase . evaluate $ M.size moduleRanks + M.size graphMap + snd (bounds modulesByRank) + snd (bounds transitiveDepRanks)
  totalModuleCount <- liftBase . evaluate $ length toBeRebuilt
  liftBase . evaluate $ foldl' (flip seq) () $
    withStrategy (parListChunk 32 rseq) [ transitiveDepRanks ! rank | (rank, _) <- toBeRebuilt ]
  void $ forConcurrentlyChunked 64 toBeRebuilt (liftBase . evaluate . CST.resFull . snd)
  sourceSizes <- forConcurrentlyChunked 64 toBeRebuilt $ \(rank, m) ->
    liftBase $ (rank,) <$> sourceSizeOrZero (T.unpack . spanName . getModuleSourceSpan . CST.resPartial $ m)
  let ownWeight = accumArray (+) 0 rankBounds sourceSizes :: Array Int Integer
  let dependentRanks = accumArray (flip (:)) [] rankBounds
        [ (dep, rank) | (rank, moduleName) <- zip [0 ..] sortedModuleNames, dep <- directDepRanks moduleName ] :: Array Int [Int]
  let downstreamWeight = listArray rankBounds
        [ ownWeight ! rank + foldl' max 0 (map (downstreamWeight !) (dependentRanks ! rank)) | rank <- range rankBounds ] :: Array Int Integer
  liftBase . evaluate $ foldl' (\() (rank, _) -> downstreamWeight ! rank `seq` ()) () toBeRebuilt
  for_ toBeRebuilt $ \(rank, m) -> fork $ do
    let moduleName = getModuleName . CST.resPartial $ m
    buildModule slots (downstreamWeight ! rank, rank) reportProgress buildPlan moduleName totalModuleCount
      (T.unpack . spanName . getModuleSourceSpan . CST.resPartial $ m)
      (fst $ CST.resFull m)
      (fmap importPrim . snd $ CST.resFull m)
      (transitiveDepRanks ! rank)
      (modulesByRank !)
      (resultsByRank !)

      -- Prevent hanging on other modules when there is an internal error
      -- (the exception is thrown, but other threads waiting on MVars are released)
      `onException` BuildPlan.markComplete buildPlan moduleName (BuildJobFailed mempty)

  -- Wait for all threads to complete, and collect results (and errors).
  (failures, successes) <-
    let
      splitResults = \case
        BuildJobSucceeded _ exts _ ->
          Right exts
        BuildJobFailed errs ->
          Left errs
        BuildJobSkipped ->
          Left mempty
    in
      M.mapEither splitResults <$> BuildPlan.collectResults buildPlan

  flushProgress progress progressQueue

  let updatedCacheDb = Cache.removeModules (M.keysSet failures) newCacheDb
  unless (totalModuleCount == 0 && updatedCacheDb == cacheDb) $
    writeCacheDb updatedCacheDb

  writePackageJson

  -- If generating docs, also generate them for the Prim modules
  outputPrimDocs
  -- All threads have completed, rethrow any caught errors.
  let errors = M.elems failures
  unless (null errors) $ throwError (mconcat errors)

  -- Here we return all the ExternsFile in the ordering of the topological sort,
  -- so they can be folded into an Environment. This result is used in the tests
  -- and in PSCI.
  let lookupResult mn@(ModuleName name) =
        fromMaybe (internalError $ "make: module not found in results: " <> T.unpack name)
        $ M.lookup mn successes

  if moCollectAllExterns then
    liftBase . evaluate . force $ map lookupResult sortedModuleNames
  else
    pure $ mapMaybe (flip M.lookup successes) sortedModuleNames

  where
  checkModuleNames :: m ()
  checkModuleNames = checkNoPrim *> checkModuleNamesAreUnique

  checkNoPrim :: m ()
  checkNoPrim =
    for_ ms $ \m ->
      let mn = getModuleName $ CST.resPartial m
      in when (isBuiltinModuleName mn) $
           throwError
             . errorMessage' (getModuleSourceSpan $ CST.resPartial m)
             $ CannotDefinePrimModules mn

  checkModuleNamesAreUnique :: m ()
  checkModuleNamesAreUnique =
    for_ (findDuplicates (getModuleName . CST.resPartial) ms) $ \mss ->
      throwError . flip foldMap mss $ \ms' ->
        let mn = getModuleName . CST.resPartial . NEL.head $ ms'
        in errorMessage'' (fmap (getModuleSourceSpan . CST.resPartial) ms') $ DuplicateModule mn

  -- Find all groups of duplicate values in a list based on a projection.
  findDuplicates :: Ord b => (a -> b) -> [a] -> Maybe [NEL.NonEmpty a]
  findDuplicates f xs =
    case filter ((> 1) . length) . NEL.groupBy ((==) `on` f) . sortOn f $ xs of
      [] -> Nothing
      xss -> Just xss

  buildModule :: BuildSlots -> (Integer, Int) -> (ProgressMessage -> m ()) -> BuildPlan -> ModuleName -> Int -> FilePath -> [CST.ParserWarning] -> Either (NEL.NonEmpty CST.ParserError) Module -> IS.IntSet -> (Int -> ModuleName) -> (Int -> BuildPlan.ResultSource) -> m ()
  buildModule slots urgency reportProgress buildPlan moduleName cnt fp pwarnings mres depRanks moduleAtRank resultAtRank = do
    result <- flip catchError (return . BuildJobFailed) $ do
      -- We need to wait for dependencies to be built, before checking if the current
      -- module should be rebuilt, so the first thing to do is to wait on the
      -- MVars for the module's dependencies.
      IS.foldr (\rank rest -> getResult (resultAtRank rank) >> rest) (pure ()) depRanks
      let deps = map moduleAtRank (IS.toAscList depRanks)
      mexterns <- fmap unzip . sequence <$> traverse (getResult . resultAtRank) (IS.toAscList depRanks)
      let pwarnings' = CST.toMultipleWarnings fp pwarnings
      let parsed = tell pwarnings' *> CST.unwrapParserError fp mres

      case mexterns of
        Just (_, depsDiffExterns) -> do
          let externs = fst <$> depsDiffExterns
          let prevResult = BuildPlan.getPrevResult buildPlan moduleName
          let depsDiffs = traverse snd depsDiffExterns
          let affectedBy diffs = not (all isEmptyDiff diffs) && either (const True) (`checkDiffs` diffs) mres
          let maySkipBuild moduleIndex
                -- We may skip built only for up-to-date modules.
                | Just (status, exts) <- prevResult
                , isUpToDate status
                -- Check if no dep's externs have changed. If any of the diffs
                -- is Nothing means we can not check and need to rebuild.
                , Just False <- affectedBy <$> depsDiffs = do
                  -- We should update modification times to mark existing
                  -- compilation results as actual. If it fails to update timestamp
                  -- on any of exiting codegen targets, it will run the build process.
                  updated <- updateOutputTimestamp moduleName
                  if updated then do
                    reportProgress $ SkippingModule moduleName moduleIndex
                    pure $ Just (exts, MultipleErrors [], Just (emptyDiff moduleName))
                  else
                    pure Nothing
                | otherwise = pure Nothing

          idx <- liftBase $ atomicModifyIORef' (bpIndex buildPlan) (\i -> (i + 1, i))

          (exts, warnings, diff) <- do
            let doBuild = do
                  m <- parsed
                  traverse_ evaluate externs
                  C.modifyMVar_ (bpEnv buildPlan) $ \env -> do
                    let missing = filter (\(dep, _) -> not (M.member dep env)) (zip deps externs)
                    C.modifyMVar_ (bpCheckEnv buildPlan) $ \checkEnv -> do
                      let checkEnv' = foldl' (flip applyExternsFileToEnvironment) checkEnv (map snd missing)
                      _ <- evaluate $ sum
                        [ M.size (names checkEnv'), M.size (types checkEnv'), M.size (dataConstructors checkEnv')
                        , M.size (typeSynonyms checkEnv'), M.size (typeClassDictionaries checkEnv'), M.size (typeClasses checkEnv')
                        ]
                      pure checkEnv'
                    foldM (\e (_, exts) -> externsEnv e exts) env missing
                  env <- C.readMVar (bpEnv buildPlan)
                  checkEnv <- C.readMVar (bpCheckEnv buildPlan)
                -- Bracket all of the per-module work behind the semaphore, including
                -- forcing the result. This is done to limit concurrency and keep
                -- memory usage down; see comments above.
                  (exts, warnings) <- bracket_ (takeBuildSlot slots urgency) (releaseBuildSlot slots) $ do
                    -- Eventlog markers for profiling; see debug/eventlog.js
                    liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " start"
                    for_ prevResult $ \(_, prev) -> when (isJust depsDiffs) $ evaluate (rnf prev)
                    -- Force the externs and warnings to avoid retaining excess module
                    -- data after the module is finished compiling.
                    extsAndWarnings <- evaluate . force <=< listen $ do
                      rebuildModuleWithIndex ma { progress = reportProgress } env checkEnv (Just (S.fromList deps)) externs m (Just (idx, cnt))
                    liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " end"
                    return extsAndWarnings
                  diff <- liftBase . evaluate $ diffExterns exts <$> (snd <$> prevResult) <*> depsDiffs
                  pure (exts, pwarnings' <> warnings, diff)
            maySkipBuild (Just (idx, cnt)) >>= maybe doBuild pure
          return $ BuildJobSucceeded warnings exts diff

        -- If we got Nothing for deps externs, that means one of the deps failed
        -- to compile. Though if we have a previous built result we will keep to
        -- avoid potentially unnecessary recompilation next time.
        Nothing -> parsed $>
          case BuildPlan.getPrevResult buildPlan moduleName of
            Just (_, exts) ->
              BuildJobSucceeded (MultipleErrors []) exts (Just (emptyDiff moduleName))
            Nothing ->
              BuildJobSkipped

    BuildPlan.markComplete buildPlan moduleName result

-- | Infer the module name for a module by looking for the same filename with
-- a .js extension.
inferForeignModules
  :: forall m
   . MonadIO m
  => M.Map ModuleName (Either RebuildPolicy FilePath)
  -> m (M.Map ModuleName FilePath)
inferForeignModules modules =
    liftIO $ M.fromDistinctAscList . catMaybes <$>
      forConcurrentlyChunked 64 (M.toAscList modules) (\(moduleName, path) -> fmap (moduleName,) <$> inferForeignModule path)
  where
    inferForeignModule :: Either RebuildPolicy FilePath -> IO (Maybe FilePath)
    inferForeignModule (Left _) = return Nothing
    inferForeignModule (Right path) = do
      let jsFile = replaceExtension path "js"
      exists <- doesFileExist jsFile
      if exists
        then return (Just jsFile)
        else return Nothing
