{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}

module Language.PureScript.Make.Index where

import Codec.Serialise (serialise)
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Distribution.Compat.Directory (makeAbsolute)
import Language.LSP.Server (MonadLsp)
import Language.PureScript.AST qualified as P
import Language.PureScript.Externs (ExternsFile (efModuleName))
import Language.PureScript.Externs qualified as P
import Language.PureScript.Lsp.NameType (LspNameType (DctorNameType), externDeclNameType, lspNameType)
import Language.PureScript.Lsp.Print (printDeclarationType, printEfDeclName, printEfDeclType, printName)
import Language.PureScript.Lsp.ServerConfig (ServerConfig)
import Language.PureScript.Lsp.Util (efDeclSourceSpan, getOperatorValueName)
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
    { P.codegen = \prevEnv astM m docs ext -> lift (indexAstModule conn astM ext (getExportedNames ext)) <* P.codegen ma prevEnv astM m docs ext
    }

indexAstModule :: (MonadIO m) => Connection -> P.Module -> ExternsFile -> Set P.Name -> m ()
indexAstModule conn (P.Module _ss _comments moduleName' decls _exportRefs) extern exportedNames = liftIO do
  path <- makeAbsolute externPath
  SQL.executeNamed
    conn
    (SQL.Query "INSERT OR REPLACE INTO ast_modules (module_name, path) VALUES (:module_name, :path)")
    [ ":module_name" := P.runModuleName moduleName',
      ":path" := path
    ]
  SQL.execute conn "DELETE FROM ast_declarations WHERE module_name = ?" (SQL.Only $ P.runModuleName moduleName')

  forM_ decls \decl -> do
    let (ss, _) = P.declSourceAnn decl
        start = P.spanStart ss
        end = P.spanEnd ss
        nameMb = P.declName decl
        nameType = nameMb <&> lspNameType
        printedType = case getOperatorValueName decl >>= disqualifyIfInModule >>= getDeclFromName of
          Nothing -> printDeclarationType decl -- TODO add check for operators in other modules
          Just decl' -> printDeclarationType decl'
    for_ nameMb \name -> do
      let exported = Set.member name exportedNames
      SQL.executeNamed
        conn
        ( SQL.Query
            "INSERT INTO ast_declarations \
            \         (module_name, name, printed_type, name_type, start_line, end_line, start_col, end_col, lines, cols, exported, generated) \
            \ VALUES (:module_name, :name, :printed_type, :name_type, :start_line, :end_line, :start_col, :end_col, :lines, :cols, :exported, :generated)"
        )
        [ ":module_name" := P.runModuleName moduleName',
          ":name" := printName name,
          ":printed_type" := printedType,
          ":name_type" := nameType,
          ":start_line" := P.sourcePosLine start,
          ":end_line" := P.sourcePosLine end,
          ":start_col" := P.sourcePosColumn start,
          ":end_col" := P.sourcePosColumn end,
          ":lines" := P.sourcePosLine end - P.sourcePosLine start,
          ":cols" := P.sourcePosColumn end - P.sourcePosColumn start,
          ":exported" := exported,
          ":generated" := "$Dict" `T.isInfixOf` printedType
        ]

      for_ (declCtrs decl) \ctr ->
        let (ss', _) = P.dataCtorAnn ctr
            start' = P.spanStart ss'
            end' = P.spanEnd ss'
         in SQL.executeNamed
              conn
              ( SQL.Query
                  "INSERT INTO ast_declarations \
                  \         (module_name, name, printed_type, name_type, start_line, end_line, start_col, end_col, lines, cols, exported, generated) \
                  \ VALUES (:module_name, :name, :printed_type, :name_type, :start_line, :end_line, :start_col, :end_col, :lines, :cols, :exported, :generated)"
              )
              [ ":module_name" := P.runModuleName moduleName',
                ":name" := P.runProperName (P.dataCtorName ctr),
                ":printed_type" := printName name,
                ":name_type" := DctorNameType,
                ":start_line" := P.sourcePosLine start',
                ":end_line" := P.sourcePosLine end',
                ":start_col" := P.sourcePosColumn start',
                ":end_col" := P.sourcePosColumn end',
                ":lines" := P.sourcePosLine end - P.sourcePosLine start',
                ":cols" := P.sourcePosColumn end - P.sourcePosColumn start',
                ":exported" := exported,
                ":generated" := "$Dict" `T.isInfixOf` printedType
              ]
  where
    externPath = P.spanName (P.efSourceSpan extern)

    getDeclFromName :: P.Name -> Maybe P.Declaration
    getDeclFromName name = find (\decl -> P.declName decl == Just name) decls

    disqualifyIfInModule :: P.Qualified P.Name -> Maybe P.Name
    disqualifyIfInModule (P.Qualified (P.ByModuleName moduleName) name) | moduleName == moduleName' = Just name
    disqualifyIfInModule (P.Qualified (P.BySourcePos _) name) = Just name
    disqualifyIfInModule _ = Nothing

declCtrs :: P.Declaration -> [P.DataConstructorDeclaration]
declCtrs = \case
  P.DataDeclaration _ _ _ _ ctors -> ctors
  _ -> []

indexAstModuleFromExtern :: (MonadIO m) => Connection -> ExternsFile -> m ()
indexAstModuleFromExtern conn extern = liftIO do
  path <- makeAbsolute externPath
  SQL.executeNamed
    conn
    (SQL.Query "INSERT OR REPLACE INTO ast_modules (module_name, path) VALUES (:module_name, :path)")
    [ ":module_name" := P.runModuleName (efModuleName extern),
      ":path" := path
    ]
  where
    externPath = P.spanName (P.efSourceSpan extern)

indexAstDeclFromExternDecl :: (MonadLsp ServerConfig m) => Connection -> ExternsFile -> Set P.Name -> P.ExternsDeclaration -> m ()
indexAstDeclFromExternDecl conn extern exportedNames externDecl = do
  let ss = case externDecl of
        P.EDDataConstructor {..}
          | Just typeCtr <- find (isTypeOfName edDataCtorTypeCtor) moduleDecls -> efDeclSourceSpan typeCtr
        _ -> efDeclSourceSpan externDecl
      start = P.spanStart ss
      end = P.spanEnd ss
      printedType :: Text
      printedType = printEfDeclType externDecl

  liftIO $
    SQL.executeNamed
      conn
      ( SQL.Query
          "INSERT INTO ast_declarations \
          \         (module_name, name, printed_type, name_type, start_line, end_line, start_col, end_col, lines, cols, exported, generated) \
          \ VALUES (:module_name, :name, :printed_type, :name_type, :start_line, :end_line, :start_col, :end_col, :lines, :cols, :exported, :generated)"
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
        ":exported" := Set.member declName exportedNames,
        ":generated" := "$Dict" `T.isInfixOf` printedType
      ]
  where
    isTypeOfName :: P.ProperName 'P.TypeName -> P.ExternsDeclaration -> Bool
    isTypeOfName name P.EDType {..} = edTypeName == name
    isTypeOfName _ _ = False

    moduleName' = efModuleName extern

    moduleDecls = P.efDeclarations extern

    declName :: P.Name
    declName = case externDecl of
      P.EDType {..} -> P.TyName edTypeName
      P.EDTypeSynonym {..} -> P.TyName edTypeSynonymName
      P.EDDataConstructor {..} -> P.DctorName edDataCtorName
      P.EDValue {..} -> P.IdentName edValueName
      P.EDClass {..} -> P.TyClassName edClassName
      P.EDInstance {..} -> P.IdentName edInstanceName

getExportedNames :: ExternsFile -> Set P.Name
getExportedNames extern =
  Set.fromList $
    P.efExports extern >>= \case
      P.TypeClassRef _ name -> [P.TyClassName name]
      P.TypeRef _ name ctrs -> [P.TyName name] <> fmap P.DctorName (fold ctrs)
      P.ValueRef _ name -> [P.IdentName name]
      P.TypeOpRef _ name -> [P.TyOpName name]
      P.ValueOpRef _ name -> [P.ValOpName name]
      P.TypeInstanceRef _ name _ -> [P.IdentName name]
      P.ModuleRef _ name -> [P.ModName name]
      P.ReExportRef _ _ _ -> []

addExternIndexing :: (MonadIO m) => Connection -> P.MakeActions m -> P.MakeActions m
addExternIndexing conn ma =
  ma
    { P.codegen = \prevEnv astM m docs ext -> lift (indexExtern conn ext) <* P.codegen ma prevEnv astM m docs ext
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
  SQL.execute_
    conn
    "CREATE TABLE IF NOT EXISTS ast_declarations \
    \(module_name TEXT references ast_modules(module_name) ON DELETE CASCADE, name TEXT, name_type TEXT, printed_type TEXT, start_line INTEGER, end_line INTEGER, start_col INTEGER, end_col INTEGER, lines INTEGER, cols INTEGER, exported BOOLEAN, generated BOOLEAN, \
    \UNIQUE(module_name, name_type, name) on conflict replace)"
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
