{-# LANGUAGE PackageImports #-}

module Language.PureScript.Lsp.Cache where

import Codec.Serialise (deserialise, serialise)
import Control.Lens (Field1 (_1), (^.), _1)
import Data.Aeson (encode)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple
import Language.PureScript qualified as P
import Language.PureScript.AST.Declarations (declSourceAnn)
import Language.PureScript.AST.Traversals (accumTypes)
import Language.PureScript.Externs (ExternsFile (efModuleName), externsFileName)
import Language.PureScript.Ide.Error (IdeError (GeneralError))
import Language.PureScript.Ide.Externs (readExternFile)
import Language.PureScript.Ide.Types (ModuleMap)
import Language.PureScript.Lsp.DB qualified as DB
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig))
import Language.PureScript.Pretty.Types (prettyPrintType)
import Protolude
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (normalise, (</>))
import "monad-logger" Control.Monad.Logger (MonadLogger)

dropTables :: (MonadIO m, MonadReader LspEnvironment m) => m ()
dropTables = do
  DB.execute_ "DROP TABLE IF EXISTS modules"
  DB.execute_ "DROP TABLE IF EXISTS declarations"
  DB.execute_ "DROP TABLE IF EXISTS externs"
  DB.execute_ "DROP TABLE IF EXISTS ef_imports"
  DB.execute_ "DROP TABLE IF EXISTS ef_exports"

initDb :: (MonadReader LspEnvironment m, MonadIO m) => m ()
initDb = do
  DB.execute_ "CREATE TABLE IF NOT EXISTS modules (module_name TEXT PRIMARY KEY, path TEXT, UNIQUE(module_name), UNIQUE(path))"
  DB.execute_ "CREATE TABLE IF NOT EXISTS declarations (module_name TEXT, name TEXT, type_printed TEXT, start_col INTEGER, start_line INTEGER, end_col INTEGER, end_line INTEGER, comments TEXT, exported BOOLEAN, value BLOB, PRIMARY KEY (module_name, name))"
  DB.execute_ "CREATE TABLE IF NOT EXISTS externs (path TEXT PRIMARY KEY, ef_version TEXT, value BLOB, module_name TEXT, UNIQUE(path), UNIQUE(module_name))"
  DB.execute_ "CREATE TABLE IF NOT EXISTS ef_imports (module_name TEXT, import_name TEXT, imported_module TEXT, import_type TEXT, imported_as TEXT)"
  DB.execute_ "CREATE TABLE IF NOT EXISTS ef_exports (module_name TEXT, export_name TEXT, value TEXT, span_name TEXT, start_col INTEGER, start_line INTEGER, end_col INTEGER, end_line INTEGER)"

selectAllExternsMap :: (MonadIO m, MonadReader LspEnvironment m) => m (ModuleMap ExternsFile)
selectAllExternsMap = do
  Map.fromList . fmap (\ef -> (efModuleName ef, ef)) <$> selectAllExterns

selectAllExterns :: (MonadIO m, MonadReader LspEnvironment m) => m [ExternsFile]
selectAllExterns = do
  DB.query_ (Query "SELECT value FROM externs") <&> fmap (deserialise . fromOnly)

insertAllExterns ::
  ( MonadIO m,
    MonadReader LspEnvironment m,
    MonadError IdeError m,
    MonadLogger m
  ) =>
  m ()
insertAllExterns = do
  oDir <- asks (confOutputPath . lspConfig)
  externPaths <- findAvailableExterns
  forM_ externPaths $ \name -> do
    extern <- readExternFile (oDir </> toS (P.runModuleName name) </> P.externsFileName)

    insertExtern oDir extern

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

insertExtern ::
  (MonadIO m, MonadReader LspEnvironment m) =>
  FilePath ->
  ExternsFile ->
  m ()
insertExtern outDir extern = do
  DB.executeNamed
    (Query "INSERT OR REPLACE INTO externs (path, ef_version, value, module_name) VALUES (:path, :ef_version, :value, :module_name)")
    [ ":path" := externsPath,
      ":ef_version" := P.efVersion extern,
      ":value" := serialise extern,
      ":module_name" := P.runModuleName name
    ]
  forM_ (P.efImports extern) $ insertEfImport name
  forM_ (P.efExports extern) $ insertEfExport name
  where
    externsPath = outDir </> T.unpack (P.runModuleName name) <> externsFileName
    name = efModuleName extern

insertEfImport :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> P.ExternsImport -> m ()
insertEfImport moduleName' ei = do
  DB.executeNamed
    (Query "INSERT OR REPLACE INTO ef_imports (module_name, imported_module, import_type, imported_as) VALUES (:module_name, :imported_module, :import_type, :imported_as)")
    [ ":module_name" := P.runModuleName moduleName',
      ":imported_module" := P.runModuleName (P.eiModule ei),
      ":import_type" := serialise (P.eiImportType ei),
      ":imported_as" := fmap P.runModuleName (P.eiImportedAs ei)
    ]

insertEfExport :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> P.DeclarationRef -> m ()
insertEfExport moduleName' dr = do
  DB.executeNamed
    (Query "INSERT OR REPLACE INTO ef_exports (module_name, value, span_name, start_col, start_line, end_col, end_line) VALUES (:module_name, :value, :span_name, :start_col, :start_line, :end_col, :end_line)")
    [ ":module_name" := P.runModuleName moduleName',
      ":value" := serialise dr,
      ":span_name" := P.spanName span,
      ":start_col" := (P.sourcePosColumn . P.spanStart) span,
      ":start_line" := (P.sourcePosLine . P.spanStart) span,
      ":end_col" := (P.sourcePosColumn . P.spanEnd) span,
      ":end_line" := (P.sourcePosLine . P.spanEnd) span
    ]
  where
    span = P.declRefSourceSpan dr

insertModule :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> P.Module -> m ()
insertModule srcPath m = do
  let moduleName' = P.getModuleName m
  DB.executeNamed
    (Query "INSERT OR REPLACE INTO modules (module_name, path) VALUES (:module_name, :path)")
    [ ":module_name" := P.runModuleName moduleName',
      ":path" := srcPath
    ]

  let exported = Set.fromList $ P.exportedDeclarations m
  traverse_ (insertDeclaration moduleName' exported) (P.getModuleDeclarations m)

insertDeclaration :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Set P.Declaration -> P.Declaration -> m ()
insertDeclaration moduleName' exportedDecls decl = do
  DB.executeNamed
    (Query "INSERT OR REPLACE INTO declarations (module_name, name, type_printed, start_col, start_line, end_col, end_line, comments, exported, value) VALUES (:module_name, :name, :type_printed, :start_col, :start_line, :end_col, :end_line, :comments, :exported, :value)")
    [ ":module_name" := P.runModuleName moduleName',
      ":name" := P.spanName declLocation,
      ":type_printed" := typeName,
      ":start_col" := (P.sourcePosColumn . P.spanStart) declLocation,
      ":start_line" := (P.sourcePosLine . P.spanStart) declLocation,
      ":end_col" := (P.sourcePosColumn . P.spanEnd) declLocation,
      ":end_line" := (P.sourcePosLine . P.spanEnd) declLocation,
      ":comments" := encode comments,
      ":exported" := exported,
      ":value" := serialise decl
    ]
  where
    typeName = Protolude.fold $ head typeNames

    typeNames :: [Text]
    typeNames = accumTypes (pure . T.pack . prettyPrintType maxBound) ^. _1 $ decl

    exported = Set.member decl exportedDecls
    (declLocation, comments) = declSourceAnn decl
