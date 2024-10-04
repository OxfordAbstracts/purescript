{-# LANGUAGE PackageImports #-}

module Language.PureScript.Lsp.Cache where

import Codec.Serialise (deserialise)
import Data.Map qualified as Map
import Data.Text qualified as T
import Database.SQLite.Simple
import Language.PureScript.Externs (ExternsFile (efModuleName))
import Language.PureScript.Externs qualified as P
import Language.PureScript.Ide.Error (IdeError (GeneralError))
import Language.PureScript.Lsp.DB qualified as DB
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig))
import Language.PureScript.Names qualified as P
import Protolude
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, makeAbsolute)
import System.FilePath (normalise, (</>))

-- import Language.PureScript.Lsp.Prim (primExterns)

selectAllExternsMap :: (MonadIO m, MonadReader LspEnvironment m) => m (Map P.ModuleName ExternsFile)
selectAllExternsMap = do
  Map.fromList . fmap (\ef -> (efModuleName ef, ef)) <$> selectAllExterns

selectAllExterns :: (MonadIO m, MonadReader LspEnvironment m) => m [ExternsFile]
selectAllExterns = do
  DB.query_ (Query "SELECT value FROM externs") <&> fmap (deserialise . fromOnly)


selectDependenciesMap :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> m (Map P.ModuleName ExternsFile)
selectDependenciesMap mName = do
  Map.fromList . fmap (\ef -> (efModuleName ef, ef)) <$> selectDependencies mName

selectDependencies :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> m [ExternsFile]
selectDependencies mName = do
  res <- DB.queryNamed (Query query') [":module_name" := P.runModuleName mName]
  pure $ deserialise . fromOnly <$> res
  where
    query' =
      unlines
        [ "with recursive",
          "graph(imported_module, level) as (",
          " select module_name , 1 as level",
          " from ef_imports where module_name = :module_name",
          " union ",
          " select d.imported_module as dep, graph.level + 1 as level",
          " from graph join ef_imports d on graph.imported_module = d.module_name",
          "),",
          "topo as (",
          " select imported_module, max(level) as level",
          " from graph group by imported_module",
          "),",
          "module_names as (select distinct(module_name)",
          "from topo join ef_imports on topo.imported_module = ef_imports.module_name ",
          "and module_name != :module_name",
          "order by level desc)",
          "select * from externs ",
          "join module_names on externs.module_name = module_names.module_name;"
        ]

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
findAvailableExterns :: (MonadIO m, MonadReader LspEnvironment m, MonadError IdeError m) => m [P.ModuleName]
findAvailableExterns = do
  oDir <- asks (confOutputPath . lspConfig)
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
