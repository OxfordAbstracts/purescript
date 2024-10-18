module Language.PureScript.Lsp.Cache where

import Codec.Serialise (deserialise)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Data.Text qualified as T
import Database.SQLite.Simple
import Language.PureScript.AST.Declarations as P
import Language.PureScript.Externs (ExternsFile (efModuleName))
import Language.PureScript.Externs qualified as P
import Language.PureScript.Glob (PSCGlobs (..), toInputGlobs, warnFileTypeNotFound)
import Language.PureScript.Ide.Error (IdeError (GeneralError))
import Language.PureScript.Lsp.DB qualified as DB
import Language.PureScript.Lsp.Types (LspEnvironment, ExternDependency)
import Language.PureScript.Names qualified as P
import Protolude
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, makeAbsolute, canonicalizePath)
import System.FilePath (normalise, (</>))
import Language.PureScript.Lsp.Log (logPerfStandard)
import Language.PureScript.Lsp.ServerConfig (ServerConfig(outputPath, globs, inputSrcFromFile))
import Language.LSP.Server (getConfig, MonadLsp)

selectAllExternsMap :: (MonadIO m, MonadReader LspEnvironment m) => m (Map P.ModuleName ExternsFile)
selectAllExternsMap = do
  Map.fromList . fmap (\ef -> (efModuleName ef, ef)) <$> selectAllExterns

selectAllExterns :: (MonadIO m, MonadReader LspEnvironment m) => m [ExternsFile]
selectAllExterns = do
  DB.query_ (Query "SELECT value FROM externs") <&> fmap (deserialise . fromOnly)


selectDependencies :: (MonadIO m, MonadReader LspEnvironment m) => P.Module -> m [ExternDependency]
selectDependencies (P.Module _ _ _ decls _) = do
  DB.queryNamed (Query query') [":module_names" := A.encode (fmap P.runModuleName importedModuleNames)]
  where
    query' =
      unlines
        [ "with recursive",
          "graph(imported_module, level) as (",
          " select module_name , 1 as level",
          " from ef_imports where module_name IN (SELECT value FROM json_each(:module_names))",
          " union ",
          " select d.imported_module as dep, graph.level + 1 as level",
          " from graph join ef_imports d on graph.imported_module = d.module_name",
          "),",
          "topo as (",
          " select imported_module, max(level) as level",
          " from graph group by imported_module",
          "),",
          "module_names as (select distinct(module_name), level",
          "from topo join ef_imports on topo.imported_module = ef_imports.module_name ",
          "order by level desc)",
          "select value, level from externs ",
          "join module_names on externs.module_name = module_names.module_name ",
          "order by level desc, module_names.module_name desc;"
        ]

    importedModuleNames =
      decls >>= \case
        P.ImportDeclaration _ importName _ _ -> [importName]
        _ -> []

selectExternFromFilePath :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> m (Maybe ExternsFile)
selectExternFromFilePath path = do
  absPath <- liftIO $ makeAbsolute path
  res <- DB.queryNamed (Query "SELECT value FROM externs WHERE path = :path") [":path" := absPath]
  pure $ deserialise . fromOnly <$> listToMaybe res

selectExternModuleNameFromFilePath :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> m (Maybe P.ModuleName)
selectExternModuleNameFromFilePath path = do
  absPath <- liftIO $ makeAbsolute path
  res <- DB.queryNamed (Query "SELECT module_name FROM externs WHERE path = :path") [":path" := absPath]
  pure $ P.ModuleName . fromOnly <$> listToMaybe res

selectExternPathFromModuleName :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> m (Maybe FilePath)
selectExternPathFromModuleName mName =
  DB.queryNamed (Query "SELECT path FROM externs WHERE module_name = :module_name") [":module_name" := P.runModuleName mName] <&> listToMaybe . fmap fromOnly

-- | Finds all the externs inside the output folder and returns the
-- corresponding module names
findAvailableExterns :: (MonadReader LspEnvironment m, MonadError IdeError m, MonadLsp ServerConfig m) => m [P.ModuleName]
findAvailableExterns = do
  oDir <- outputPath <$> getConfig
  unlessM
    (liftIO (doesDirectoryExist oDir))
    (throwError (GeneralError $ "Couldn't locate your output directory at: " <> T.pack (normalise oDir)))
  liftIO $ do
    directories <- getDirectoryContents oDir
    moduleNames <- filterM (containsExterns oDir) directories
    pure (P.moduleNameFromString . toS <$> moduleNames)
  where
    -- Takes the output directory and a filepath like "Data.Array" and
    -- looks up, whether that folder contains an externs file
    containsExterns :: FilePath -> FilePath -> IO Bool
    containsExterns oDir d
      | d `elem` [".", ".."] = pure False
      | otherwise = do
          let file = oDir </> d </> P.externsFileName
          doesFileExist file

updateAvailableSrcs :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m) => m [FilePath]
updateAvailableSrcs = logPerfStandard "updateAvailableSrcs" $ do
  DB.execute_ "CREATE TABLE IF NOT EXISTS available_srcs (path TEXT PRIMARY KEY NOT NULL, UNIQUE(path) on conflict replace)"
  DB.execute_ (Query "DELETE FROM available_srcs")
  config <- getConfig
  srcs <-
    liftIO $
      toInputGlobs $
        PSCGlobs
          { pscInputGlobs = globs config,
            pscInputGlobsFromFile = inputSrcFromFile config,
            pscExcludeGlobs = [],
            pscWarnFileTypeNotFound = warnFileTypeNotFound "lsp server"
          }
  for_ srcs $ \src -> do
    canonPath <- liftIO $ canonicalizePath src
    DB.executeNamed (Query "INSERT INTO available_srcs (path) VALUES (:path)") [":path" := canonPath]
    absPath <- liftIO $ makeAbsolute src
    when (absPath /= canonPath) $
      DB.executeNamed (Query "INSERT INTO available_srcs (path) VALUES (:path)") [":path" := absPath]

  pure srcs