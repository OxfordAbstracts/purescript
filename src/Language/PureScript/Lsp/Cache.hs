{-# LANGUAGE PackageImports #-}

module Language.PureScript.Lsp.Cache where

import Codec.Serialise (deserialise, serialise)
import Data.Aeson (encode)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple
import Language.PureScript.AST qualified as P
import Language.PureScript.AST.Declarations (declName, declRefName, declSourceAnn)
import Language.PureScript.AST.SourcePos (SourceSpan (spanName))
import Language.PureScript.Externs (ExternsFile (efModuleName, efSourceSpan))
import Language.PureScript.Externs qualified as P
import Language.PureScript.Ide.Error (IdeError (GeneralError))
import Language.PureScript.Ide.Externs (readExternFile)
import Language.PureScript.Lsp.DB qualified as DB
import Language.PureScript.Lsp.Print (printEfDeclName, printName)
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig))
import Language.PureScript.Lsp.Util (efDeclCategory, efDeclSourceSpan)
import Language.PureScript.Names qualified as P
import Protolude
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, makeAbsolute)
import System.FilePath (normalise, (</>))
import "monad-logger" Control.Monad.Logger (MonadLogger)
-- import Language.PureScript.Lsp.Prim (primExterns)

dropTables :: (MonadIO m, MonadReader LspEnvironment m) => m ()
dropTables = do
  DB.execute_ "DROP TABLE IF EXISTS modules"
  DB.execute_ "DROP TABLE IF EXISTS declarations"
  DB.execute_ "DROP TABLE IF EXISTS externs"
  DB.execute_ "DROP TABLE IF EXISTS ef_imports"
  DB.execute_ "DROP TABLE IF EXISTS ef_exports"
  DB.execute_ "DROP TABLE IF EXISTS ef_declarations"

initDb :: (MonadReader LspEnvironment m, MonadIO m) => m ()
initDb = do
  DB.execute_ "CREATE TABLE IF NOT EXISTS modules (module_name TEXT PRIMARY KEY, path TEXT, UNIQUE(module_name), UNIQUE(path))"
  DB.execute_ "CREATE TABLE IF NOT EXISTS declarations (module_name TEXT, name BLOB, printed_name TEXT, type_printed TEXT, start_col INTEGER, start_line INTEGER, end_col INTEGER, end_line INTEGER, comments TEXT, exported BOOLEAN, value BLOB, shown TEXT, PRIMARY KEY (module_name, name))"
  DB.execute_ "CREATE TABLE IF NOT EXISTS externs (path TEXT PRIMARY KEY, ef_version TEXT, value BLOB, module_name TEXT, shown TEXT, UNIQUE(path), UNIQUE(module_name))"
  DB.execute_ "CREATE TABLE IF NOT EXISTS ef_imports (module_name TEXT, imported_module TEXT, import_type TEXT, imported_as TEXT, value BLOB)"
  DB.execute_ "CREATE TABLE IF NOT EXISTS ef_exports (module_name TEXT, export_name TEXT, value BLOB, name BLOB, printed_name TEXT, start_col INTEGER, start_line INTEGER, end_col INTEGER, end_line INTEGER)"
  DB.execute_ "CREATE TABLE IF NOT EXISTS ef_declarations (module_name TEXT, name TEXT, value BLOB, start_col INTEGER, start_line INTEGER, end_col INTEGER, end_line INTEGER, category TEXT, shown TEXT)"
  -- traverse_ insertExtern primExterns

selectAllExternsMap :: (MonadIO m, MonadReader LspEnvironment m) => m (Map P.ModuleName ExternsFile)
selectAllExternsMap = do
  Map.fromList . fmap (\ef -> (efModuleName ef, ef)) <$> selectAllExterns

selectAllExterns :: (MonadIO m, MonadReader LspEnvironment m) => m [ExternsFile]
selectAllExterns = do
  DB.query_ (Query "SELECT value FROM externs") <&> fmap (deserialise . fromOnly)

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
    let externPath = oDir </> toS (P.runModuleName name) </> P.externsFileName
    extern <- readExternFile externPath
    insertExtern extern

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
  ExternsFile ->
  m ()
insertExtern extern = do
  path <- liftIO $ makeAbsolute externPath
  DB.executeNamed
    (Query "INSERT OR REPLACE INTO externs (path, ef_version, value, module_name, shown) VALUES (:path, :ef_version, :value, :module_name, :shown)")
    [ ":path" := path,
      ":ef_version" := P.efVersion extern,
      ":value" := serialise extern,
      ":module_name" := P.runModuleName name,
      ":shown" := (show extern :: Text)
    ]

  DB.executeNamed
    (Query "DELETE FROM ef_imports WHERE module_name = :module_name")
    [":module_name" := P.runModuleName name]
  forM_ (P.efImports extern) $ insertEfImport name
  DB.executeNamed
    (Query "DELETE FROM ef_exports WHERE module_name = :module_name")
    [":module_name" := P.runModuleName name]
  forM_ (P.efExports extern) $ insertEfExport name
  DB.executeNamed
    (Query "DELETE FROM ef_declarations WHERE module_name = :module_name")
    [":module_name" := P.runModuleName name]
  forM_ (P.efDeclarations extern) $ insertEfDeclaration name
  where
    name = efModuleName extern
    externPath = spanName (efSourceSpan extern)

insertEfImport :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> P.ExternsImport -> m ()
insertEfImport moduleName' ei = do
  DB.executeNamed
    (Query "INSERT OR REPLACE INTO ef_imports (module_name, imported_module, import_type, imported_as, value) VALUES (:module_name, :imported_module, :import_type, :imported_as, :value)")
    [ ":module_name" := P.runModuleName moduleName',
      ":imported_module" := P.runModuleName (P.eiModule ei),
      ":import_type" := serialise (P.eiImportType ei),
      ":imported_as" := fmap P.runModuleName (P.eiImportedAs ei),
      ":value" := serialise ei
    ]

insertEfDeclaration :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> P.ExternsDeclaration -> m ()
insertEfDeclaration moduleName' decl = do
  DB.executeNamed
    (Query "INSERT OR REPLACE INTO ef_declarations (module_name, value, shown, name, start_col, start_line, end_col, end_line, category) VALUES (:module_name, :value, :shown, :name, :start_col, :start_line, :end_col, :end_line, :category)")
    [ ":module_name" := P.runModuleName moduleName',
      ":name" := printEfDeclName decl,
      ":value" := serialise decl,
      ":shown" := (show decl :: Text),
      ":start_col" := (P.sourcePosColumn . P.spanStart) span,
      ":start_line" := (P.sourcePosLine . P.spanStart) span,
      ":end_col" := (P.sourcePosColumn . P.spanEnd) span,
      ":end_line" := (P.sourcePosLine . P.spanEnd) span,
      ":category" := efDeclCategory decl
    ]
  where
    span = efDeclSourceSpan decl

insertEfExport :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> P.DeclarationRef -> m ()
insertEfExport moduleName' dr = do
  DB.executeNamed
    (Query "INSERT OR REPLACE INTO ef_exports (module_name, value, name, printed_name, start_col, start_line, end_col, end_line) VALUES (:module_name, :value, :name, :printed_name, :start_col, :start_line, :end_col, :end_line)")
    [ ":module_name" := P.runModuleName moduleName',
      ":value" := serialise dr,
      ":name" := serialise (declRefName dr),
      ":printed_name" := printName (declRefName dr),
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
  DB.executeNamed "DELETE FROM declarations WHERE module_name = :module_name" [":module_name" := P.runModuleName moduleName']
  traverse_ (insertDeclaration moduleName' exported) (P.getModuleDeclarations m)

insertDeclaration :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Set P.Declaration -> P.Declaration -> m ()
insertDeclaration moduleName' exportedDecls decl = do
  for_ (declName decl) $ \name -> do
    DB.executeNamed
      ( Query
          "INSERT OR REPLACE INTO declarations \
          \(module_name, name, printed_name, start_col, start_line, end_col, end_line, comments, exported, value, shown) \
          \VALUES \
          \(:module_name, :name, :printed_name, :start_col, :start_line, :end_col, :end_line, :comments, :exported, :value, :shown)"
      )
      [ ":module_name" := P.runModuleName moduleName',
        ":name" := serialise name,
        ":printed_name" := printName name,
        ":start_col" := (P.sourcePosColumn . P.spanStart) declLocation,
        ":start_line" := (P.sourcePosLine . P.spanStart) declLocation,
        ":end_col" := (P.sourcePosColumn . P.spanEnd) declLocation,
        ":end_line" := (P.sourcePosLine . P.spanEnd) declLocation,
        ":comments" := encode comments,
        ":exported" := exported,
        ":value" := serialise decl,
        ":shown" := (show decl :: Text)
      ]
  where
    exported = Set.member decl exportedDecls
    (declLocation, comments) = declSourceAnn decl
