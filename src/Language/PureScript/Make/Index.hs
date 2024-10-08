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

addAllIndexing :: (MonadIO m) => Connection -> P.MakeActions m -> P.MakeActions m
addAllIndexing conn ma =
  addAstModuleIndexing conn $
    -- addEnvIndexing conn $
    addCoreFnIndexing conn $
      addExternIndexing conn ma

addAstModuleIndexing :: (MonadIO m) => Connection -> P.MakeActions m -> P.MakeActions m
addAstModuleIndexing conn ma =
  ma
    { P.codegen = \prevEnv env astM m docs ext -> lift (indexAstModule conn astM ext) <* P.codegen ma prevEnv env astM m docs ext
    }

indexAstModule :: (MonadIO m) => Connection -> P.Module -> ExternsFile -> m ()
indexAstModule conn m@(P.Module _ss _comments name decls exportRefs) extern = liftIO do
  path <- makeAbsolute externPath
  SQL.executeNamed
    conn
    (SQL.Query "INSERT OR REPLACE INTO ast_modules (module_name, path) VALUES (:module_name, :path)")
    [ ":module_name" := P.runModuleName name,
      ":path" := path
    ]
  SQL.execute conn "DELETE FROM ast_declarations WHERE module_name = ?" (SQL.Only $ P.runModuleName name)
  SQL.execute conn "DELETE FROM ast_expressions WHERE module_name = ?" (SQL.Only $ P.runModuleName name)

  let exports = Set.fromList $ P.exportedDeclarations m

  forM_ decls \decl -> do
    let (ss, _) = P.declSourceAnn decl
    let start = P.spanStart ss
        end = P.spanEnd ss
    SQL.executeNamed
      conn
      (SQL.Query "INSERT INTO ast_declarations (module_name, name, value, printed_type, start_line, end_line, start_col, end_col, lines, cols, exported) VALUES (:module_name, :name, :value, :printed_type, :start_line, :end_line, :start_col, :end_col, :lines, :cols, :exported)")
      [ ":module_name" := P.runModuleName name,
        ":name" := printName <$> P.declName decl,
        ":value" := serialise decl,
        ":printed_type" := printDeclarationType decl,
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

insertDeclExprs :: (MonadIO m) => Connection -> P.ModuleName -> P.Declaration -> m ()
insertDeclExprs conn name decl = liftIO $ void $ handleDecl decl
  where
    (handleDecl, _, _) =
      P.everywhereOnValuesM
        pure
        (\e -> e <$ insertAstExpr e)
        pure

    insertAstExpr :: P.Expr -> IO ()
    insertAstExpr expr =
      SQL.execute
        conn
        (SQL.Query "INSERT INTO ast_expressions (module_name, value, shown, start_line, end_line, start_col, end_col, length) VALUES (?, ?, ?, ?, ?, ?, ?, ?)")
        ( P.runModuleName name,
          serialise expr,
          show expr :: Text,
          fmap (P.sourcePosLine . P.spanStart) ss,
          fmap (P.sourcePosLine . P.spanEnd) ss,
          fmap (P.sourcePosColumn . P.spanStart) ss,
          fmap (P.sourcePosColumn . P.spanEnd) ss,
          T.length (show expr :: Text)
        )
      where
        ss = exprSourceSpan expr

addEnvIndexing :: (MonadIO m) => Connection -> P.MakeActions m -> P.MakeActions m
addEnvIndexing conn ma =
  ma
    { P.codegen = \prevEnv env astM m docs ext -> lift (indexEnv conn (P.getModuleName astM) env) <* P.codegen ma prevEnv env astM m docs ext
    }

indexEnv :: (MonadIO m) => Connection -> P.ModuleName -> P.Environment -> m ()
indexEnv conn name env =
  liftIO $
    SQL.executeNamed
      conn
      (SQL.Query "INSERT OR REPLACE INTO envs (module_name, value) VALUES (:module_name, :value)")
      [ ":module_name" := P.runModuleName name,
        ":value" := serialise env
      ]

addCoreFnIndexing :: (MonadIO m) => Connection -> P.MakeActions m -> P.MakeActions m
addCoreFnIndexing conn ma =
  ma
    { P.codegen = \prevEnv env astM m docs ext -> lift (indexCoreFn conn m) <* P.codegen ma prevEnv env astM m docs ext
    }

indexCoreFn :: forall m. (MonadIO m) => Connection -> CF.Module CF.Ann -> m ()
indexCoreFn conn m = do
  liftIO do
    let mName = P.runModuleName $ CF.moduleName m
    path <- makeAbsolute $ CF.modulePath m
    SQL.execute conn "DELETE FROM corefn_modules WHERE name = ?" (SQL.Only mName)
    SQL.execute
      conn
      (SQL.Query "INSERT INTO corefn_modules (name, path, value) VALUES (?, ?, ?)")
      ( mName,
        path,
        A.encode $ CFJ.moduleToJSON Paths.version m
      )

    SQL.execute conn "DELETE FROM corefn_imports WHERE module_name = ?" (SQL.Only mName)
    SQL.execute conn "DELETE FROM corefn_declarations WHERE module_name = ?" (SQL.Only mName)
    SQL.execute conn "DELETE FROM corefn_expressions WHERE module_name = ?" (SQL.Only mName)

    forM_ (CF.moduleImports m) \((span, _, _), importedModule) -> do
      SQL.execute
        conn
        (SQL.Query "INSERT INTO corefn_imports (module_name, imported_module) VALUES (?, ?)")
        ( mName,
          P.runModuleName importedModule
        )

    forM_ (CF.moduleDecls m) \b ->
      do
        let insertBindQuery topLevel ss ident bind =
              SQL.execute
                conn
                ( SQL.Query
                    "INSERT INTO corefn_declarations (module_name, ident, top_level, value, start_line, end_line, start_col, end_col) \
                    \VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                )
                ( mName,
                  P.runIdent ident,
                  topLevel,
                  A.encode $ CFJ.bindToJSON bind,
                  P.sourcePosLine $ P.spanStart ss,
                  P.sourcePosLine $ P.spanEnd ss,
                  P.sourcePosColumn $ P.spanStart ss,
                  P.sourcePosColumn $ P.spanEnd ss
                )
            (handleBind, handleExpr, handleBinder, handleCaseAlternative) =
              traverseCoreFn (insertBind False) insertExpr handleBinder handleCaseAlternative

            insertBind' :: Bool -> CF.Bind CF.Ann -> IO ()
            insertBind' topLevel bind = do
              case bind of
                CF.NonRec (ss, _comments, _meta) ident expr -> do
                  insertBindQuery topLevel ss ident bind
                CF.Rec binds -> forM_ binds $ \(((ss, _, _), ident), expr) -> do
                  insertBindQuery topLevel ss ident bind

            insertBind :: Bool -> CF.Bind CF.Ann -> IO (CF.Bind CF.Ann)
            insertBind topLevel bind = do
              insertBind' topLevel bind
              handleBind bind

            insertExpr :: CF.Expr CF.Ann -> IO (CF.Expr CF.Ann)
            insertExpr expr = do
              SQL.execute
                conn
                ( SQL.Query
                    "INSERT INTO corefn_expressions (module_name, value, start_line, end_line, start_col, end_col, lines, cols, shown)\
                    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
                )
                ( mName,
                  A.encode $ CFJ.exprToJSON expr,
                  P.sourcePosLine start,
                  P.sourcePosLine end,
                  P.sourcePosColumn start,
                  P.sourcePosColumn end,
                  lines',
                  cols,
                  show expr :: Text
                )
              handleExpr expr
              where
                (ss, _comments, _meta) = CF.extractAnn expr
                start = P.spanStart ss
                end = P.spanEnd ss
                lines' = P.sourcePosLine end - P.sourcePosLine start
                cols = P.sourcePosColumn end - P.sourcePosColumn start

        void $ insertBind' True b
        void $ handleBind b

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
  -- dropTables conn
  SQL.execute_ conn "pragma journal_mode=wal;"
  SQL.execute_ conn "pragma foreign_keys = ON;"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ast_modules (module_name TEXT, path TEXT, UNIQUE(module_name) on conflict replace, UNIQUE(path) on conflict replace)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ast_declarations (module_name TEXT, name TEXT, value TEXT, printed_type TEXT, start_line INTEGER, end_line INTEGER, start_col INTEGER, end_col INTEGER, lines INTEGER, cols INTEGER, exported BOOLEAN)"
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
  SQL.execute_ conn "DROP TABLE IF EXISTS envs"
  SQL.execute_ conn "DROP TABLE IF EXISTS corefn_modules"
  SQL.execute_ conn "DROP TABLE IF EXISTS corefn_imports"
  SQL.execute_ conn "DROP TABLE IF EXISTS corefn_declarations"
  SQL.execute_ conn "DROP TABLE IF EXISTS corefn_expressions"
  SQL.execute_ conn "DROP TABLE IF EXISTS externs"
  SQL.execute_ conn "DROP TABLE IF EXISTS ef_imports"
  SQL.execute_ conn "DROP TABLE IF EXISTS ef_exports"
  SQL.execute_ conn "DROP TABLE IF EXISTS ef_declarations"

-- xzzz =
--   TypedValue
--     True
--     ( Var
--         (SS {ss = SP {l = 15, c = 31}, end = SP {l = 15, c = 34}})
--         ( Qualified
--             ( ByModuleName
--                 (ModuleName "Data.Functor")
--             )
--             (Ident "map")
--         )
--     )
--     ( ForAll
--         (SS {spanName = "", ss = SP {l = 0, c = 0}, end = SP {l = 0, c = 0}}, [])
--         TypeVarVisible
--         "f"
--         ( Just
--             ( TypeApp
--                 (SS {spanName = "", ss = SP {l = 0, c = 0}, end = SP {l = 0, c = 0}}, [])
--                 ( TypeApp
--                     (SS {spanName = "", ss = SP {l = 0, c = 0}, end = SP {l = 0, c = 0}}, [])
--                     ( TypeConstructor
--                         (SS {spanName = "", ss = SP {l = 0, c = 0}, end = SP {l = 0, c = 0}}, [])
--                         ( Qualified
--                             ( ByModuleName
--                                 (ModuleName "Prim")
--                             )
--                             (ProperName {runProperName = "Function"})
--                         )
--                     )
--                     ( TypeConstructor
--                         (SS {spanName = "", ss = SP {l = 0, c = 0}, end = SP {l = 0, c = 0}}, [])
--                         ( Qualified
--                             ( ByModuleName
--                                 (ModuleName "Prim")
--                             )
--                             (ProperName {runProperName = "Type"})
--                         )
--                     )
--                 )
--                 ( TypeConstructor
--                     (SS {spanName = "", ss = SP {l = 0, c = 0}, end = SP {l = 0, c = 0}}, [])
--                     ( Qualified
--                         ( ByModuleName
--                             (ModuleName "Prim")
--                         )
--                         (ProperName {runProperName = "Type"})
--                     )
--                 )
--             )
--         )
--         ( ForAll
--             (SS {ss = SP {l = 32, c = 10}, end = SP {l = 32, c = 44}}, [])
--             TypeVarInvisible
--             "a"
--             ( Just
--                 ( TypeConstructor
--                     (SS {ss = SP {l = 32, c = 25}, end = SP {l = 32, c = 27}}, [])
--                     ( Qualified
--                         ( ByModuleName
--                             (ModuleName "Prim")
--                         )
--                         (ProperName {runProperName = "Type"})
--                     )
--                 )
--             )
--             ( ForAll
--                 (SS {ss = SP {l = 32, c = 19}, end = SP {l = 32, c = 44}}, [])
--                 TypeVarInvisible
--                 "b"
--                 ( Just
--                     ( TypeConstructor
--                         (SS {ss = SP {l = 32, c = 25}, end = SP {l = 32, c = 27}}, [])
--                         ( Qualified
--                             ( ByModuleName
--                                 (ModuleName "Prim")
--                             )
--                             (ProperName {runProperName = "Type"})
--                         )
--                     )
--                 )
--                 ( ConstrainedType
--                     (SS {spanName = "", ss = SP {l = 0, c = 0}, end = SP {l = 0, c = 0}}, [])
--                     ( Constraint
--                         { constraintAnn =
--                             (SS {spanName = "", ss = SP {l = 0, c = 0}, end = SP {l = 0, c = 0}}, []),
--                           constraintClass =
--                             Qualified
--                               ( ByModuleName
--                                   (ModuleName "Data.Functor")
--                               )
--                               (ProperName {runProperName = "Functor"}),
--                           constraintKindArgs = [],
--                           constraintArgs =
--                             [ TypeVar
--                                 (SS {spanName = "", ss = SP {l = 0, c = 0}, end = SP {l = 0, c = 0}}, [])
--                                 "f"
--                             ],
--                           constraintData = Nothing
--                         }
--                     )
--                     ( TypeApp
--                         (SS {ss = SP {l = 32, c = 22}, end = SP {l = 32, c = 44}}, [])
--                         ( TypeApp
--                             (SS {ss = SP {l = 32, c = 22}, end = SP {l = 32, c = 44}}, [])
--                             ( TypeConstructor
--                                 (SS {ss = SP {l = 32, c = 31}, end = SP {l = 32, c = 33}}, [])
--                                 ( Qualified
--                                     ( ByModuleName
--                                         (ModuleName "Prim")
--                                     )
--                                     (ProperName {runProperName = "Function"})
--                                 )
--                             )
--                             ( TypeApp
--                                 (SS {ss = SP {l = 32, c = 23}, end = SP {l = 32, c = 29}}, [])
--                                 ( TypeApp
--                                     (SS {ss = SP {l = 32, c = 23}, end = SP {l = 32, c = 29}}, [])
--                                     ( TypeConstructor
--                                         (SS {ss = SP {l = 32, c = 25}, end = SP {l = 32, c = 27}}, [])
--                                         ( Qualified
--                                             ( ByModuleName
--                                                 (ModuleName "Prim")
--                                             )
--                                             (ProperName {runProperName = "Function"})
--                                         )
--                                     )
--                                     ( TypeVar
--                                         (SS {ss = SP {l = 32, c = 23}, end = SP {l = 32, c = 24}}, [])
--                                         "a"
--                                     )
--                                 )
--                                 ( TypeVar
--                                     (SS {ss = SP {l = 32, c = 28}, end = SP {l = 32, c = 29}}, [])
--                                     "b"
--                                 )
--                             )
--                         )
--                         ( TypeApp
--                             (SS {ss = SP {l = 32, c = 34}, end = SP {l = 32, c = 44}}, [])
--                             ( TypeApp
--                                 (SS {ss = SP {l = 32, c = 34}, end = SP {l = 32, c = 44}}, [])
--                                 ( TypeConstructor
--                                     (SS {ss = SP {l = 32, c = 38}, end = SP {l = 32, c = 40}}, [])
--                                     ( Qualified
--                                         ( ByModuleName
--                                             (ModuleName "Prim")
--                                         )
--                                         (ProperName {runProperName = "Function"})
--                                     )
--                                 )
--                                 ( TypeApp
--                                     (SS {ss = SP {l = 32, c = 34}, end = SP {l = 32, c = 37}}, [])
--                                     ( TypeVar
--                                         (SS {ss = SP {l = 32, c = 34}, end = SP {l = 32, c = 35}}, [])
--                                         "f"
--                                     )
--                                     ( TypeVar
--                                         (SS {ss = SP {l = 32, c = 36}, end = SP {l = 32, c = 37}}, [])
--                                         "a"
--                                     )
--                                 )
--                             )
--                             ( TypeApp
--                                 (SS {ss = SP {l = 32, c = 41}, end = SP {l = 32, c = 44}}, [])
--                                 ( TypeVar
--                                     (SS {ss = SP {l = 32, c = 41}, end = SP {l = 32, c = 42}}, [])
--                                     "f"
--                                 )
--                                 ( TypeVar
--                                     (SS {ss = SP {l = 32, c = 43}, end = SP {l = 32, c = 44}}, [])
--                                     "b"
--                                 )
--                             )
--                         )
--                     )
--                 )
--                 ( Just
--                     (SkS {rss = 0})
--                 )
--             )
--             ( Just
--                 (SkS {rss = 1})
--             )
--         )
--         ( Just
--             (SkS {rss = 2})
--         )
--     )
