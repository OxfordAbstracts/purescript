{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.PureScript.Make.Index where

import Codec.Serialise (serialise)
import Control.Exception (handle)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Supply (SupplyT (SupplyT))
import Data.Aeson qualified as A
import Data.List qualified as List
import Data.Map.Lazy qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Distribution.Compat.Directory (makeAbsolute)
import Language.PureScript (declRefName)
import Language.PureScript.AST qualified as P
import Language.PureScript.AST.Declarations (exprSourceSpan)
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.AST.Traversals qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.CoreFn qualified as CF
import Language.PureScript.CoreFn.FromJSON qualified as CFJ
import Language.PureScript.CoreFn.ToJSON qualified as CFJ
import Language.PureScript.CoreFn.Traversals (traverseCoreFn)
import Language.PureScript.Docs.Types qualified as Docs
import Language.PureScript.Environment qualified as P
import Language.PureScript.Errors qualified as P
import Language.PureScript.Externs (ExternsFile (efModuleName))
import Language.PureScript.Externs qualified as P
import Language.PureScript.Ide.Error (IdeError (GeneralError, RebuildError))
import Language.PureScript.Ide.Rebuild (updateCacheDb)
import Language.PureScript.Ide.Types (ModuleMap)
import Language.PureScript.Ide.Util (ideReadFile)
import Language.PureScript.Lsp.Print (printDeclarationType, printEfDeclName, printName)
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig))
import Language.PureScript.Lsp.Util (efDeclCategory, efDeclSourceSpan)
import Language.PureScript.Make (ffiCodegen')
import Language.PureScript.Make qualified as P
import Language.PureScript.ModuleDependencies qualified as P
import Language.PureScript.Names qualified as P
import Language.PureScript.Options qualified as P
import Language.PureScript.Types (everywhereOnTypesM)
import Paths_purescript qualified as Paths
import Protolude hiding (moduleName)
import "monad-logger" Control.Monad.Logger (MonadLogger, logDebugN)
import Language.PureScript.Lsp.NameType (lspNameType)

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
indexAstModule conn m@(P.Module _ss _comments moduleName' decls exportRefs) extern = liftIO do
  path <- makeAbsolute externPath
  SQL.executeNamed
    conn
    (SQL.Query "INSERT OR REPLACE INTO ast_modules (module_name, path) VALUES (:module_name, :path)")
    [ ":module_name" := P.runModuleName moduleName',
      ":path" := path
    ]
  SQL.execute conn "DELETE FROM ast_declarations WHERE module_name = ?" (SQL.Only $ P.runModuleName moduleName')
  SQL.execute conn "DELETE FROM ast_expressions WHERE module_name = ?" (SQL.Only $ P.runModuleName moduleName')

  let exports = Set.fromList $ P.exportedDeclarations m

  forM_ decls \decl -> do
    let (ss, _) = P.declSourceAnn decl
        start = P.spanStart ss
        end = P.spanEnd ss
        name = P.declName decl
        nameType =  name <&> lspNameType
    SQL.executeNamed
      conn
      (SQL.Query 
      "INSERT INTO ast_declarations \
      \         (module_name, name, printed_type, name_type, start_line, end_line, start_col, end_col, lines, cols, exported) \
      \ VALUES (:module_name, :name, :printed_type, :name_type, :start_line, :end_line, :start_col, :end_col, :lines, :cols, :exported)")
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
    (SQL.Query "INSERT OR REPLACE INTO externs (path, ef_version, value, module_name, shown) VALUES (:path, :ef_version, :value, :module_name, :shown)")
    [ ":path" := path,
      ":ef_version" := P.efVersion extern,
      ":value" := serialise extern,
      ":module_name" := P.runModuleName name,
      ":shown" := (show extern :: Text)
    ]
  forM_ (P.efImports extern) $ insertEfImport conn name
  forM_ (P.efExports extern) $ insertEfExport conn name
  forM_ (P.efDeclarations extern) $ insertEfDeclaration conn name
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

insertEfDeclaration :: Connection -> P.ModuleName -> P.ExternsDeclaration -> IO ()
insertEfDeclaration conn moduleName' decl = do
  SQL.executeNamed
    conn
    (SQL.Query "INSERT OR REPLACE INTO ef_declarations (module_name, value, shown, name, start_col, start_line, end_col, end_line, category) VALUES (:module_name, :value, :shown, :name, :start_col, :start_line, :end_col, :end_line, :category)")
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

insertEfExport :: Connection -> P.ModuleName -> P.DeclarationRef -> IO ()
insertEfExport conn moduleName' dr = do
  SQL.executeNamed
    conn
    (SQL.Query "INSERT OR REPLACE INTO ef_exports (module_name, value, name, printed_name, start_col, start_line, end_col, end_line) VALUES (:module_name, :value, :name, :printed_name, :start_col, :start_line, :end_col, :end_line)")
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

initDb :: Connection -> IO ()
initDb conn = do
  dropTables conn
  SQL.execute_ conn "pragma journal_mode=wal;"
  SQL.execute_ conn "pragma foreign_keys = ON;"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ast_modules (module_name TEXT, path TEXT, UNIQUE(module_name) on conflict replace, UNIQUE(path) on conflict replace)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ast_declarations (module_name TEXT, name TEXT, name_type TEXT, printed_type TEXT, start_line INTEGER, end_line INTEGER, start_col INTEGER, end_col INTEGER, lines INTEGER, cols INTEGER, exported BOOLEAN)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ast_expressions (module_name TEXT, value TEXT, shown TEXT, start_line INTEGER, end_line INTEGER, start_col INTEGER, end_col INTEGER, length INTEGER)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS envs (module_name TEXT PRIMARY KEY, value TEXT)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS corefn_modules (name TEXT PRIMARY KEY, path TEXT, value TEXT, UNIQUE(name) on conflict replace, UNIQUE(path) on conflict replace)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS corefn_imports (module_name TEXT references corefn_modules(name) ON DELETE CASCADE, imported_module TEXT, UNIQUE(module_name, imported_module) on conflict replace)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS corefn_declarations (module_name TEXT references corefn_modules(name) ON DELETE CASCADE, ident TEXT, top_level BOOLEAN, value TEXT, start_line INTEGER, end_line INTEGER, start_col INTEGER, end_col INTEGER)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS corefn_expressions (module_name TEXT references corefn_modules(name) ON DELETE CASCADE, value TEXT, start_line INTEGER, end_line INTEGER, start_col INTEGER, end_col INTEGER, lines INTEGER, cols INTEGER, shown TEXT)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS externs (path TEXT PRIMARY KEY, ef_version TEXT, value BLOB, module_name TEXT, shown TEXT, UNIQUE(path) on conflict replace, UNIQUE(module_name) on conflict replace)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ef_imports (module_name TEXT references externs(module_name) ON DELETE CASCADE, imported_module TEXT, import_type TEXT, imported_as TEXT, value BLOB)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ef_exports (module_name TEXT references externs(module_name) ON DELETE CASCADE, export_name TEXT, value BLOB, name BLOB, printed_name TEXT, start_col INTEGER, start_line INTEGER, end_col INTEGER, end_line INTEGER)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ef_declarations (module_name TEXT references externs(module_name) ON DELETE CASCADE, name TEXT, value BLOB, start_col INTEGER, start_line INTEGER, end_col INTEGER, end_line INTEGER, category TEXT, shown TEXT)"

  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS available_srcs (path TEXT PRIMARY KEY NOT NULL, UNIQUE(path) on conflict replace)"

  addDbIndexes conn

addDbIndexes :: Connection -> IO ()
addDbIndexes conn = do
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_declarations_module_name ON ast_declarations (module_name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_declarations_name ON ast_declarations (name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_declarations_start_line ON ast_declarations (start_line)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_declarations_end_line ON ast_declarations (end_line)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_expressions_start_line ON ast_expressions (start_line)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_expressions_end_line ON ast_expressions (end_line)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS corefn_modules_name ON corefn_modules (name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS corefn_modules_path ON corefn_modules (path)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS corefn_imports_module ON corefn_imports (module_name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS corefn_imports_imported_module ON corefn_imports (imported_module)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS corefn_declarations_module_name ON corefn_declarations (module_name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS corefn_declarations_start_line ON corefn_declarations (start_line)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS corefn_declarations_end_line ON corefn_declarations (end_line)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS corefn_expressions_start_line ON corefn_expressions (start_line)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS corefn_expressions_end_line ON corefn_expressions (end_line)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS corefn_expressions_lines ON corefn_expressions (lines)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS corefn_expressions_cols ON corefn_expressions (cols)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS corefn_expressions_module_name ON corefn_expressions (module_name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS externs_path ON externs (path)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS externs_module_name ON externs (module_name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ef_imports_module_name ON ef_imports (module_name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ef_imports_imported_module ON ef_imports (imported_module)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ef_imports_import_type ON ef_imports (import_type)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ef_imports_imported_as ON ef_imports (imported_as)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ef_exports_module_name ON ef_exports (module_name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ef_exports_export_name ON ef_exports (export_name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ef_declarations_module_name ON ef_declarations (module_name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ef_declarations_name ON ef_declarations (name)"

dropTables :: Connection -> IO ()
dropTables conn = do
  SQL.execute_ conn "DROP TABLE IF EXISTS ast_declarations"
  SQL.execute_ conn "DROP TABLE IF EXISTS ast_expressions"
  SQL.execute_ conn "DROP TABLE IF EXISTS ast_modules"
  SQL.execute_ conn "DROP TABLE IF EXISTS envs"
  SQL.execute_ conn "DROP TABLE IF EXISTS corefn_modules"
  SQL.execute_ conn "DROP TABLE IF EXISTS corefn_imports"
  SQL.execute_ conn "DROP TABLE IF EXISTS corefn_declarations"
  SQL.execute_ conn "DROP TABLE IF EXISTS corefn_expressions"
  SQL.execute_ conn "DROP TABLE IF EXISTS externs"
  SQL.execute_ conn "DROP TABLE IF EXISTS ef_imports"
  SQL.execute_ conn "DROP TABLE IF EXISTS ef_exports"
  SQL.execute_ conn "DROP TABLE IF EXISTS ef_declarations"
