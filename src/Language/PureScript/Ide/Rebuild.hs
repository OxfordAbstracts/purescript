{-# language PackageImports, TemplateHaskell, BlockArguments #-}

module Language.PureScript.Ide.Rebuild
  ( rebuildFileSync
  , rebuildFileAsync
  , rebuildFile
  ) where

import Protolude hiding (moduleName)

import "monad-logger" Control.Monad.Logger (LoggingT, MonadLogger, logDebug)
import Data.List qualified as List
import Data.Map.Lazy qualified as M
import Data.Maybe (fromJust, catMaybes)
import Data.Set qualified as S
import Data.Time qualified as Time
import Data.Text qualified as Text
import Language.PureScript qualified as P
import Language.PureScript.Make (ffiCodegen')
import Language.PureScript.Make.Cache (CacheInfo(..), normaliseForCache)
import Language.PureScript.CST qualified as CST

import Language.PureScript.Ide.Error (IdeError(..))
import Language.PureScript.Ide.Logging (labelTimespec, logPerf, runLogger)
import Language.PureScript.Ide.State (cacheRebuild, getExternFiles, insertExterns, insertModule, populateVolatileState, updateCacheTimestamp, runQuery)
import Language.PureScript.Ide.Types (Ide, IdeConfiguration(..), IdeEnvironment(..), ModuleMap, Success(..))
import Language.PureScript.Ide.Util (ideReadFile)
import System.Directory (getCurrentDirectory)
import Database.SQLite.Simple qualified as SQLite
import System.FilePath ((</>))
import Data.Aeson (decode)
import Language.PureScript.Externs (ExternsFile(ExternsFile))
import Data.ByteString qualified as T
import Data.ByteString.Lazy qualified as TE
import Language.PureScript.Names (runModuleName)
import Data.Text (intercalate)
import Unsafe.Coerce (unsafeCoerce)
import Database.SQLite.Simple (Query(fromQuery), ToRow, SQLData (SQLText))
import Data.String (String)
import Codec.Serialise (deserialise)
import System.FilePath (makeRelative)

-- | Given a filepath performs the following steps:
--
-- * Reads and parses a PureScript module from the filepath.
--
-- * Builds a dependency graph for the parsed module from the already loaded
-- ExternsFiles.
--
-- * Attempts to find an FFI definition file for the module by looking
-- for a file with the same filepath except for a .js extension.
--
-- * Passes all the created artifacts to @rebuildModule@.
--
-- * If the rebuilding succeeds, returns a @RebuildSuccess@ with the generated
-- warnings, and if rebuilding fails, returns a @RebuildError@ with the
-- generated errors.
rebuildFile
  :: (Ide m, MonadLogger m, MonadError IdeError m)
  => FilePath
  -- ^ The file to rebuild
  -> Maybe FilePath
  -- ^ The file to use as the location for parsing and errors
  -> Set P.CodegenTarget
  -- ^ The targets to codegen
  -> (ReaderT IdeEnvironment (LoggingT IO) () -> m ())
  -- ^ A runner for the second build with open exports
  -> m Success
rebuildFile file actualFile codegenTargets runOpenBuild = do
  currentDir <- liftIO getCurrentDirectory
  (fp, input) <-
    case List.stripPrefix "data:" file of
      Just source -> pure ("", Text.pack source)
      _ -> ideReadFile file
  let fp' = fromMaybe fp actualFile
  (pwarnings, m) <- case sequence $ CST.parseFromFile fp' input of
    Left parseError ->
      throwError $ RebuildError [(fp', input)] $ CST.toMultipleErrors fp' parseError
    Right m -> pure m
  let moduleName = P.getModuleName m
  outputDirectory <- confOutputPath . ideConfiguration <$> ask
  -- Externs files must be sorted ahead of time, so that they get applied
  -- in the right order (bottom up) to the 'Environment'.
  -- externs' <- logPerf (labelTimespec "Sorting externs") (sortExterns m =<< getExternFiles)
  !externs <- logPerf (labelTimespec "Sorting externs") (sortExterns' outputDirectory m)
  -- For rebuilding, we want to 'RebuildAlways', but for inferring foreign
  -- modules using their file paths, we need to specify the path in the 'Map'.
  let filePathMap = M.singleton moduleName (Left P.RebuildAlways)
  let pureRebuild = fp == ""
  let modulePath = if pureRebuild then fp' else file
  foreigns <- P.inferForeignModules (M.singleton moduleName (Right modulePath))
  let makeEnv = P.buildMakeActions outputDirectory filePathMap foreigns False
        & (if pureRebuild then enableForeignCheck foreigns codegenTargets . shushCodegen else identity)
        & shushProgress
  -- Rebuild the single module using the cached externs
  (result, warnings) <- logPerf (labelTimespec "Rebuilding Module") $
    liftIO $ P.runMake (P.defaultOptions { P.optionsCodegenTargets = codegenTargets }) do
      newExterns <- P.rebuildModule makeEnv externs m
      unless pureRebuild
        $ updateCacheDb codegenTargets outputDirectory file actualFile moduleName
      pure newExterns
  case result of
    Left errors ->
      throwError (RebuildError [(fp', input)] errors)
    Right newExterns -> do
      -- insertModule (fromMaybe file actualFile, m)
      -- insertExterns newExterns
      -- void populateVolatileState
      _ <- updateCacheTimestamp
      -- runOpenBuild (rebuildModuleOpen makeEnv externs m)
      pure (RebuildSuccess (CST.toMultipleWarnings fp pwarnings <> warnings))

-- | When adjusting the cache db file after a rebuild we always pick a
-- non-sensical timestamp ("1858-11-17T00:00:00Z"), and rely on the
-- content hash to tell whether the module needs rebuilding. This is
-- because IDE rebuilds may be triggered on temporary files to not
-- force editors to save the actual source file to get at diagnostics
dayZero :: Time.UTCTime
dayZero = Time.UTCTime (Time.ModifiedJulianDay 0) 0

updateCacheDb
  :: MonadIO m
  => MonadError P.MultipleErrors m
  => Set P.CodegenTarget
  -> FilePath
  -- ^ The output directory
  -> FilePath
  -- ^ The file to read the content hash from
  -> Maybe FilePath
  -- ^ The file name to update in the cache
  -> P.ModuleName
  -- ^ The module name to update in the cache
  -> m ()
updateCacheDb codegenTargets outputDirectory file actualFile moduleName = do
  cwd <- liftIO getCurrentDirectory
  contentHash <- P.hashFile file
  let moduleCacheInfo = (normaliseForCache cwd (fromMaybe file actualFile), (dayZero, contentHash))

  foreignCacheInfo <-
    if S.member P.JS codegenTargets then do
      foreigns' <- P.inferForeignModules (M.singleton moduleName (Right (fromMaybe file actualFile)))
      for (M.lookup moduleName foreigns') \foreignPath -> do
        foreignHash <- P.hashFile foreignPath
        pure (normaliseForCache cwd foreignPath, (dayZero, foreignHash))
    else
      pure Nothing

  let cacheInfo = M.fromList (moduleCacheInfo : maybeToList foreignCacheInfo)
  cacheDb <- P.readCacheDb' outputDirectory
  P.writeCacheDb' outputDirectory (M.insert moduleName (CacheInfo cacheInfo) cacheDb)

rebuildFileAsync
  :: forall m. (Ide m, MonadLogger m, MonadError IdeError m)
  => FilePath -> Maybe FilePath -> Set P.CodegenTarget -> m Success
rebuildFileAsync fp fp' ts = rebuildFile fp fp' ts asyncRun
  where
    asyncRun :: ReaderT IdeEnvironment (LoggingT IO) () -> m ()
    asyncRun action = do
        env <- ask
        let ll = confLogLevel (ideConfiguration env)
        void (liftIO (async (runLogger ll (runReaderT action env))))

rebuildFileSync
  :: forall m. (Ide m, MonadLogger m, MonadError IdeError m)
  => FilePath -> Maybe FilePath -> Set P.CodegenTarget -> m Success
rebuildFileSync fp fp' ts = rebuildFile fp fp' ts syncRun
  where
    syncRun :: ReaderT IdeEnvironment (LoggingT IO) () -> m ()
    syncRun action = do
        env <- ask
        let ll = confLogLevel (ideConfiguration env)
        void (liftIO (runLogger ll (runReaderT action env)))

-- | Rebuilds a module but opens up its export list first and stores the result
-- inside the rebuild cache
rebuildModuleOpen
  :: (Ide m, MonadLogger m)
  => P.MakeActions P.Make
  -> [P.ExternsFile]
  -> P.Module
  -> m ()
rebuildModuleOpen makeEnv externs m = void $ runExceptT do
  (openResult, _) <- liftIO $ P.runMake P.defaultOptions $
    P.rebuildModule (shushProgress (shushCodegen makeEnv)) externs (openModuleExports m)
  case openResult of
    Left _ ->
      throwError (GeneralError "Failed when rebuilding with open exports")
    Right result -> do
      $(logDebug)
        ("Setting Rebuild cache: " <> P.runModuleName (P.efModuleName result))
      cacheRebuild result

-- | Shuts the compiler up about progress messages
shushProgress :: Monad m => P.MakeActions m -> P.MakeActions m
shushProgress ma =
  ma { P.progress = \_ -> pure () }

-- | Stops any kind of codegen
shushCodegen :: Monad m => P.MakeActions m -> P.MakeActions m
shushCodegen ma =
  ma { P.codegen = \_ _ _ _ -> pure ()
     , P.ffiCodegen = \_ -> pure ()
     }

-- | Enables foreign module check without actual codegen.
enableForeignCheck
  :: M.Map P.ModuleName FilePath
  -> S.Set P.CodegenTarget
  -> P.MakeActions P.Make
  -> P.MakeActions P.Make
enableForeignCheck foreigns codegenTargets ma =
  ma { P.ffiCodegen = ffiCodegen' foreigns codegenTargets Nothing
     }

-- | Returns a topologically sorted list of dependent ExternsFiles for the given
-- module. Throws an error if there is a cyclic dependency within the
-- ExternsFiles
sortExterns
  :: (Ide m, MonadError IdeError m)
  => P.Module
  -> ModuleMap P.ExternsFile
  -> m [P.ExternsFile]
sortExterns m ex = do
  sorted' <- runExceptT
           . P.sortModules P.Transitive P.moduleSignature
           . (:) m
           . map mkShallowModule
           . M.elems
           . M.delete (P.getModuleName m) $ ex
  case sorted' of
    Left err ->
      throwError (RebuildError [] err)
    Right (sorted, graph) -> do
      let deps = fromJust (List.lookup (P.getModuleName m) graph)
      pure $ mapMaybe getExtern (deps `inOrderOf` map P.getModuleName sorted)
  where
    mkShallowModule P.ExternsFile{..} =
      P.Module (P.internalModuleSourceSpan "<rebuild>") [] efModuleName (map mkImport efImports) Nothing
    mkImport (P.ExternsImport mn it iq) =
      P.ImportDeclaration (P.internalModuleSourceSpan "<rebuild>", []) mn it iq
    getExtern mn = M.lookup mn ex
    -- Sort a list so its elements appear in the same order as in another list.
    inOrderOf :: (Ord a) => [a] -> [a] -> [a]
    inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

sortExterns'
  :: (Ide m)
  => FilePath
  -> P.Module
  -> m [P.ExternsFile]
sortExterns' _ m = do 
  let P.Module _ _ _ declarations _ = m
  let moduleDependencies = declarations >>= \case
              P.ImportDeclaration _ importName _ _ -> [importName]
              _ -> []

  externs <- runQuery $ unlines [
           "with recursive",
           "graph(dependency, level) as (", 
           " select module_name , 1 as level",
           " from modules where module_name in (" <> Data.Text.intercalate ", " (moduleDependencies <&> \v -> "'" <> runModuleName v <> "'") <> ")", 
           " union ",
           " select d.dependency as dep, graph.level + 1 as level", 
           " from graph join dependencies d on graph.dependency = d.module_name",  
           "),",
           "topo as (", 
           " select dependency, max(level) as level", 
           " from graph group by dependency", 
           ") ", 
           "select extern",
           "from topo join modules on topo.dependency = modules.module_name order by level desc;"
          ]

  pure $ (externs >>= identity) <&> deserialise 

  -- !r <- SQLite.withConnection (outputDir </> "cache.db") \conn -> 
  --   SQLite.query conn query (SQLite.Only $ "[" <> Data.Text.intercalate ", " (dependencies <&> \v -> "\"" <> runModuleName v <> "\"") <> "]")
  --     <&> \r -> (r >>= identity) <&> deserialise 
  -- pure r

-- | Removes a modules export list.
openModuleExports :: P.Module -> P.Module
openModuleExports (P.Module ss cs mn decls _) = P.Module ss cs mn decls Nothing
