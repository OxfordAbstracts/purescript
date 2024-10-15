{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}

module Language.PureScript.Make.Index where

import Codec.Serialise (serialise)
import Data.Set qualified as Set
import Database.SQLite.Simple (Connection, NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Distribution.Compat.Directory (makeAbsolute)
import Language.PureScript.AST qualified as P
import Language.PureScript.Externs (ExternsFile (efModuleName))
import Language.PureScript.Externs qualified as P
import Language.PureScript.Lsp.NameType (externDeclNameType, lspNameType)
import Language.PureScript.Lsp.Print (printDeclarationType, printEfDeclName, printEfDeclType, printName)
import Language.PureScript.Lsp.Util (efDeclSourceSpan)
import Language.PureScript.Make qualified as P
import Language.PureScript.Names qualified as P
import Protolude hiding (moduleName)

addAllIndexing :: (MonadIO m) => Connection -> P.MakeActions m -> P.MakeActions m
addAllIndexing conn ma =
  addAstModuleIndexing conn $
    addExternIndexing conn ma

addAstModuleIndexing :: (MonadIO m) => Connection -> P.MakeActions m -> P.MakeActions m
addAstModuleIndexing conn ma =
  ma
    { P.codegen = \prevEnv env astM m docs ext -> lift (indexAstModule conn astM ext) <* P.codegen ma prevEnv env astM m docs ext
    }

indexAstModule :: (MonadIO m) => Connection -> P.Module -> ExternsFile -> m ()
indexAstModule conn m@(P.Module _ss _comments moduleName' decls _exportRefs) extern = liftIO do
  path <- makeAbsolute externPath
  SQL.executeNamed
    conn
    (SQL.Query "INSERT OR REPLACE INTO ast_modules (module_name, path) VALUES (:module_name, :path)")
    [ ":module_name" := P.runModuleName moduleName',
      ":path" := path
    ]
  SQL.execute conn "DELETE FROM ast_declarations WHERE module_name = ?" (SQL.Only $ P.runModuleName moduleName')

  let exports = Set.fromList $ P.exportedDeclarations m

  forM_ decls \decl -> do
    let (ss, _) = P.declSourceAnn decl
        start = P.spanStart ss
        end = P.spanEnd ss
        name = P.declName decl
        nameType = name <&> lspNameType
    SQL.executeNamed
      conn
      ( SQL.Query
          "INSERT INTO ast_declarations \
          \         (module_name, name, printed_type, name_type, start_line, end_line, start_col, end_col, lines, cols, exported) \
          \ VALUES (:module_name, :name, :printed_type, :name_type, :start_line, :end_line, :start_col, :end_col, :lines, :cols, :exported)"
      )
      [ ":module_name" := P.runModuleName moduleName',
        ":name" := printName <$> name,
        ":printed_type" := printDeclarationType decl,
        ":name_type" := nameType,
        ":start_line" := P.sourcePosLine start,
        ":end_line" := P.sourcePosLine end,
        ":start_col" := P.sourcePosColumn start,
        ":end_col" := P.sourcePosColumn end,
        ":lines" := P.sourcePosLine end - P.sourcePosLine start,
        ":cols" := P.sourcePosColumn end - P.sourcePosColumn start,
        ":exported" := Set.member decl exports
      ]
  where
    externPath = P.spanName (P.efSourceSpan extern)

indexAstDeclFromExternDecl :: (MonadIO m) => Connection -> P.ModuleName -> [P.ExternsDeclaration] -> P.ExternsDeclaration -> m ()
indexAstDeclFromExternDecl conn moduleName' moduleDecls externDecl = liftIO do
  let ss = case externDecl of
        P.EDDataConstructor {..}
          | Just typeCtr <- find (isTypeOfName edDataCtorTypeCtor) moduleDecls -> efDeclSourceSpan typeCtr
        _ -> efDeclSourceSpan externDecl
      start = P.spanStart ss
      end = P.spanEnd ss
      printedType :: Text
      printedType = printEfDeclType externDecl
  SQL.executeNamed
    conn
    ( SQL.Query
        "INSERT INTO ast_declarations \
        \         (module_name, name, printed_type, name_type, start_line, end_line, start_col, end_col, lines, cols, exported) \
        \ VALUES (:module_name, :name, :printed_type, :name_type, :start_line, :end_line, :start_col, :end_col, :lines, :cols, :exported)"
    )
    [ ":module_name" := P.runModuleName moduleName',
      ":name" := printEfDeclName externDecl,
      ":printed_type" := printedType,
      ":name_type" := externDeclNameType externDecl,
      ":start_line" := P.sourcePosLine start,
      ":end_line" := P.sourcePosLine end,
      ":start_col" := P.sourcePosColumn start,
      ":end_col" := P.sourcePosColumn end,
      ":lines" := P.sourcePosLine end - P.sourcePosLine start,
      ":cols" := P.sourcePosColumn end - P.sourcePosColumn start,
      ":exported" := False
    ]
  where
    isTypeOfName :: P.ProperName 'P.TypeName -> P.ExternsDeclaration -> Bool
    isTypeOfName name P.EDType {..} = edTypeName == name
    isTypeOfName _ _ = False

addExternIndexing :: (MonadIO m) => Connection -> P.MakeActions m -> P.MakeActions m
addExternIndexing conn ma =
  ma
    { P.codegen = \prevEnv env astM m docs ext -> lift (indexExtern conn ext) <* P.codegen ma prevEnv env astM m docs ext
    }

indexExtern :: (MonadIO m) => Connection -> ExternsFile -> m ()
indexExtern conn extern = liftIO do
  path <- liftIO $ makeAbsolute externPath
  SQL.executeNamed
    conn
    (SQL.Query "DELETE FROM externs WHERE path = :path")
    [":path" := path]
  SQL.executeNamed
    conn
    (SQL.Query "INSERT OR REPLACE INTO externs (path, ef_version, value, module_name) VALUES (:path, :ef_version, :value, :module_name)")
    [ ":path" := path,
      ":ef_version" := P.efVersion extern,
      ":value" := serialise extern,
      ":module_name" := P.runModuleName name
    ]
  forM_ (P.efImports extern) $ insertEfImport conn name
  where
    name = efModuleName extern
    externPath = P.spanName (P.efSourceSpan extern)

insertEfImport :: Connection -> P.ModuleName -> P.ExternsImport -> IO ()
insertEfImport conn moduleName' ei = do
  SQL.executeNamed
    conn
    (SQL.Query "INSERT OR REPLACE INTO ef_imports (module_name, imported_module, import_type, imported_as, value) VALUES (:module_name, :imported_module, :import_type, :imported_as, :value)")
    [ ":module_name" := P.runModuleName moduleName',
      ":imported_module" := P.runModuleName (P.eiModule ei),
      ":import_type" := serialise (P.eiImportType ei),
      ":imported_as" := fmap P.runModuleName (P.eiImportedAs ei),
      ":value" := serialise ei
    ]

initDb :: Connection -> IO ()
initDb conn = do
  SQL.execute_ conn "pragma journal_mode=wal;"
  SQL.execute_ conn "pragma foreign_keys = ON;"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ast_modules (module_name TEXT, path TEXT, UNIQUE(module_name) on conflict replace, UNIQUE(path) on conflict replace)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ast_declarations (module_name TEXT, name TEXT, name_type TEXT, printed_type TEXT, start_line INTEGER, end_line INTEGER, start_col INTEGER, end_col INTEGER, lines INTEGER, cols INTEGER, exported BOOLEAN)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS externs (path TEXT PRIMARY KEY, ef_version TEXT, value BLOB, module_name TEXT, UNIQUE(path) on conflict replace, UNIQUE(module_name) on conflict replace)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ef_imports (module_name TEXT references externs(module_name) ON DELETE CASCADE, imported_module TEXT, import_type TEXT, imported_as TEXT, value BLOB)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS available_srcs (path TEXT PRIMARY KEY NOT NULL, UNIQUE(path) on conflict replace)"

  addDbIndexes conn

addDbIndexes :: Connection -> IO ()
addDbIndexes conn = do
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_declarations_module_name ON ast_declarations (module_name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_declarations_name ON ast_declarations (name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_declarations_start_line ON ast_declarations (start_line)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_declarations_end_line ON ast_declarations (end_line)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS externs_path ON externs (path)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS externs_module_name ON externs (module_name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ef_imports_module_name ON ef_imports (module_name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ef_imports_imported_module ON ef_imports (imported_module)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ef_imports_import_type ON ef_imports (import_type)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ef_imports_imported_as ON ef_imports (imported_as)"

dropTables :: Connection -> IO ()
dropTables conn = do
  SQL.execute_ conn "DROP TABLE IF EXISTS ast_declarations"
  SQL.execute_ conn "DROP TABLE IF EXISTS ast_modules"
  SQL.execute_ conn "DROP TABLE IF EXISTS externs"
  SQL.execute_ conn "DROP TABLE IF EXISTS ef_imports"
