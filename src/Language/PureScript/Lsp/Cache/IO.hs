module Language.PureScript.Lsp.Cache.IO where

import Protolude
import Database.SQLite.Simple qualified as SQL


dropTables :: SQL.Connection -> IO ()
dropTables conn = do
  SQL.execute_ conn "DROP TABLE IF EXISTS modules"
  SQL.execute_ conn "DROP TABLE IF EXISTS declarations"
  SQL.execute_ conn "DROP TABLE IF EXISTS externs"
  SQL.execute_ conn "DROP TABLE IF EXISTS ef_imports"
  SQL.execute_ conn "DROP TABLE IF EXISTS ef_exports"
  SQL.execute_ conn "DROP TABLE IF EXISTS ef_declarations"

initDb :: SQL.Connection -> IO ()
initDb conn = do
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS modules (module_name TEXT PRIMARY KEY, path TEXT, UNIQUE(module_name), UNIQUE(path))"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS declarations (module_name TEXT, name BLOB, printed_name TEXT, type_printed TEXT, start_col INTEGER, start_line INTEGER, end_col INTEGER, end_line INTEGER, comments TEXT, exported BOOLEAN, value BLOB, shown TEXT, PRIMARY KEY (module_name, name))"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS externs (path TEXT PRIMARY KEY, ef_version TEXT, value BLOB, module_name TEXT, shown TEXT, UNIQUE(path), UNIQUE(module_name))"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ef_imports (module_name TEXT, imported_module TEXT, import_type TEXT, imported_as TEXT, value BLOB)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ef_exports (module_name TEXT, export_name TEXT, value BLOB, name BLOB, printed_name TEXT, start_col INTEGER, start_line INTEGER, end_col INTEGER, end_line INTEGER)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ef_declarations (module_name TEXT, name TEXT, value BLOB, start_col INTEGER, start_line INTEGER, end_col INTEGER, end_line INTEGER, category TEXT, shown TEXT)"
