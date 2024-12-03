{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}

module Language.PureScript.Make.Index
  ( initDb,
    addAllIndexing,
    addAstModuleIndexing,
    addExternIndexing,
    indexAstModuleFromExtern,
    indexAstDeclFromExternDecl,
    dropTables,
    indexExtern,
    getExportedNames,
    insertEnvValue,
    insertType,
    insertDataConstructor,
    insertTypeSynonym,
  )
where

import Codec.Serialise (serialise)
import Control.Concurrent.Async.Lifted (mapConcurrently_)
import Data.List (partition)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, NamedParam ((:=)), type (:.) (..))
import Database.SQLite.Simple qualified as SQL
import Distribution.Compat.Directory (makeAbsolute)
import Language.LSP.Server (MonadLsp)
import Language.PureScript qualified as P
import Language.PureScript.AST.Declarations (DeclarationRef)
import Language.PureScript.Environment (Environment)
import Language.PureScript.Environment qualified as E
import Language.PureScript.Externs (ExternsFile (efModuleName))
import Language.PureScript.Lsp.NameType (LspNameType (DctorNameType), declNameType, externDeclNameType, lspNameType)
import Language.PureScript.Lsp.Print (addDataDeclArgKind, printCtrType, printDataDeclKind, printDeclarationType, printEfDeclName, printEfDeclType, printName, printType, printTypeClassKind)
import Language.PureScript.Lsp.ServerConfig (ServerConfig)
import Language.PureScript.Lsp.Util (efDeclSourceSpan, getOperatorValueName)
import Language.PureScript.Names (Qualified ())
import Language.PureScript.TypeChecker.Monad (emptyCheckState)
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope (tcdClassName, tcdValue))
import Protolude hiding (moduleName)
import Data.Aeson qualified as A
import Language.PureScript.Make.Index.Select (toDbQualifer)

addAllIndexing :: (MonadIO m) => Connection -> P.MakeActions m -> P.MakeActions m
addAllIndexing conn ma =
  addAstModuleIndexing conn $
    addEnvIndexing conn $
      addExternIndexing conn ma

addAstModuleIndexing :: (MonadIO m) => Connection -> P.MakeActions m -> P.MakeActions m
addAstModuleIndexing conn ma =
  ma
    { P.codegen = \prevEnv checkSt astM m docs ext ->
        lift (indexAstModule conn (P.checkEnv checkSt) astM ext (getExportedNames ext)) <* P.codegen ma prevEnv checkSt astM m docs ext
    }

addEnvIndexing :: (MonadIO m) => Connection -> P.MakeActions m -> P.MakeActions m
addEnvIndexing conn ma =
  ma
    { P.codegen = \prevEnv checkSt astM@(P.Module _ _ _ _ refs) m docs ext -> do
        lift (indexExportedEnv (P.getModuleName astM) (P.checkEnv checkSt) refs conn)
        P.codegen ma prevEnv checkSt astM m docs ext
    }

indexAstModule :: (MonadIO m) => Connection -> Environment -> P.Module -> ExternsFile -> Set P.Name -> m ()
indexAstModule conn endEnv (P.Module _ss _comments moduleName' decls _exportRefs) extern exportedNames = liftIO do
  path <- makeAbsolute externPath
  SQL.executeNamed
    conn
    (SQL.Query "INSERT OR REPLACE INTO ast_modules (module_name, path) VALUES (:module_name, :path)")
    [ ":module_name" := P.runModuleName moduleName',
      ":path" := path
    ]

  SQL.execute conn "DELETE FROM ast_declarations WHERE module_name = ?" (SQL.Only $ P.runModuleName moduleName')

  let declsSorted :: [P.Declaration]
      declsSorted = partition (not . isTypeDecl) decls & uncurry (<>)

      isTypeDecl = \case
        P.TypeDeclaration _ -> True
        _ -> False

  forM_ declsSorted \decl -> do
    indexFixity conn moduleName' decl
    let (ss, _) = P.declSourceAnn decl
        start = P.spanStart ss
        end = P.spanEnd ss
        nameMb = P.declName decl
        getMatchingKind sigFor tyName = findMap (\case P.KindDeclaration _ sigFor' name kind | sigFor == sigFor' && name == tyName -> Just kind; _ -> Nothing) decls
        getPrintedType d = case getOperatorValueName d >>= disqualifyIfInModule >>= getDeclFromName of
          Just decl' -> printDeclarationType decl'
          Nothing -> case d of
            P.DataDeclaration _ _ tyName args _ -> case getMatchingKind P.DataSig tyName of
              Just kind -> printType kind
              _ -> printDataDeclKind args
            P.TypeSynonymDeclaration ann name args ty -> case getMatchingKind P.TypeSynonymSig name of
              Just kind -> printType kind
              _ ->
                let addForall ty' = foldl' (\acc v -> P.ForAll P.nullSourceAnn P.TypeVarInvisible v Nothing acc Nothing) ty' vars
                      where
                        vars = P.usedTypeVariables ty'

                    inferSynRes =
                      runExcept $ evalStateT (P.inferKind . addForall =<< P.inferTypeSynonym moduleName' (ann, name, args, ty)) (emptyCheckState endEnv) {P.checkCurrentModule = Just moduleName'}
                 in case inferSynRes of
                      Left err -> "Inference error: " <> T.pack (P.prettyPrintMultipleErrors P.noColorPPEOptions err)
                      Right (_, tyKind) ->
                        printType $ foldr addDataDeclArgKind (void tyKind) args
            P.TypeClassDeclaration _ name args _ _ _ -> case getMatchingKind P.ClassSig (P.coerceProperName name) of
              Just kind -> printType kind
              _ -> printTypeClassKind args
            _ -> printDeclarationType d

    let printedType = getPrintedType decl

    for_ nameMb \name -> do
      let exported = Set.member name exportedNames
          nameType = fromMaybe (lspNameType name) $ declNameType decl
          printedName = printName name

      SQL.executeNamed
        conn
        ( SQL.Query
            "INSERT INTO ast_declarations \
            \         (module_name, name, printed_type, name_type, decl_ctr, start_line, end_line, start_col, end_col, lines, cols, exported, generated) \
            \ VALUES (:module_name, :name, :printed_type, :name_type, :decl_ctr, :start_line, :end_line, :start_col, :end_col, :lines, :cols, :exported, :generated)"
        )
        [ ":module_name" := P.runModuleName moduleName',
          ":name" := printedName,
          ":printed_type" := printedType,
          ":name_type" := nameType,
          ":decl_ctr" := P.declCtr decl,
          ":start_line" := P.sourcePosLine start,
          ":end_line" := P.sourcePosLine end,
          ":start_col" := P.sourcePosColumn start,
          ":end_col" := P.sourcePosColumn end,
          ":lines" := P.sourcePosLine end - P.sourcePosLine start,
          ":cols" := P.sourcePosColumn end - P.sourcePosColumn start,
          ":exported" := exported,
          ":generated" := "$Dict" `T.isInfixOf` printedType
        ]
      for_ (declCtrs decl) $
        \(sa, tyName, ctrs) ->
          for_ ctrs $ \ctr -> do
            let (ss', _) = P.dataCtorAnn ctr
                start' = P.spanStart ss'
                end' = P.spanEnd ss'
                ctrPrintedType = printCtrType (P.spanStart $ fst sa) tyName ctr

            SQL.executeNamed
              conn
              ( SQL.Query
                  "INSERT INTO ast_declarations \
                  \         (module_name, name, printed_type, name_type, ctr_type, start_line, end_line, start_col, end_col, lines, cols, exported, generated) \
                  \ VALUES (:module_name, :name, :printed_type, :name_type, :ctr_type, :start_line, :end_line, :start_col, :end_col, :lines, :cols, :exported, :generated)"
              )
              [ ":module_name" := P.runModuleName moduleName',
                ":name" := P.runProperName (P.dataCtorName ctr),
                ":printed_type" := ctrPrintedType,
                ":name_type" := DctorNameType,
                ":ctr_type" := printedName,
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

indexFixity :: Connection -> P.ModuleName -> P.Declaration -> IO ()
indexFixity conn moduleName' = \case
  P.FixityDeclaration _ (Left (P.ValueFixity (P.Fixity assoc prec) (P.Qualified (P.ByModuleName val_mod) name) op)) -> 
    SQL.executeNamed
      conn
      ( SQL.Query
          "INSERT INTO value_operators (module_name, op_name, alias_module_name, alias, associativity, precedence) \
          \ VALUES (:module_name, :op_name, :alias_module_name, :alias, :associativity, :precedence)"
      )
      [ ":module_name" :=  P.runModuleName moduleName',
        ":op_name" := P.runOpName op,
        ":alias_module_name" :=  P.runModuleName val_mod,
        ":alias" := either P.runIdent P.runProperName name,
        ":associativity" := P.showAssoc assoc,
        ":precedence" := prec
      ]
  P.FixityDeclaration _ (Right (P.TypeFixity (P.Fixity assoc prec) (P.Qualified (P.ByModuleName ty_mod) name) op)) ->
    SQL.executeNamed 
     conn 
     ( SQL.Query
         "INSERT INTO type_operators (module_name, op_name, alias_module_name, alias, associativity, precedence) \
         \ VALUES (:module_name, :op_name, :alias_module_name, :alias, :associativity, :precedence)"
     )
     [ ":module_name" :=  P.runModuleName moduleName',
       ":op_name" := P.runOpName op,
       ":alias_module_name" :=  P.runModuleName ty_mod,
       ":alias" := A.encode name,
       ":associativity" := P.showAssoc assoc,
       ":precedence" := prec
     ]
  _ -> pure ()

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = listToMaybe . mapMaybe f

declCtrs :: P.Declaration -> Maybe (P.SourceAnn, P.ProperName 'P.TypeName, [P.DataConstructorDeclaration])
declCtrs = \case
  P.DataDeclaration sa _ n _ ctors -> Just (sa, n, ctors)
  _ -> Nothing

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
    { P.codegen = \prevEnv endEnv astM m docs ext -> lift (indexExtern conn ext) <* P.codegen ma prevEnv endEnv astM m docs ext
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
    (SQL.Query "INSERT OR REPLACE INTO externs (path, ef_version, value, hash, module_name) VALUES (:path, :ef_version, :value, :hash, :module_name)")
    [ ":path" := path,
      ":ef_version" := P.efVersion extern,
      ":value" := serialised,
      ":hash" := hash serialised,
      ":module_name" := P.runModuleName name
    ]
  forM_ (P.efImports extern) $ insertEfImport conn name
  where
    name = efModuleName extern
    externPath = P.spanName (P.efSourceSpan extern)
    serialised = serialise extern

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
  SQL.execute_ conn "pragma foreign_keys=ON;"
  SQL.execute_ conn "pragma cache_size=-6000;"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ast_modules (module_name TEXT, path TEXT, UNIQUE(module_name) on conflict replace, UNIQUE(path) on conflict replace)"
  SQL.execute_
    conn
    "CREATE TABLE IF NOT EXISTS ast_declarations \
    \(module_name TEXT references ast_modules(module_name) ON DELETE CASCADE, name TEXT, name_type TEXT, decl_ctr TEXT, ctr_type TEXT, printed_type TEXT, start_line INTEGER, end_line INTEGER, start_col INTEGER, end_col INTEGER, lines INTEGER, cols INTEGER, exported BOOLEAN, generated BOOLEAN, \
    \UNIQUE(module_name, name_type, name) on conflict replace)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS externs (path TEXT PRIMARY KEY, hash INT NOT NULL, ef_version TEXT, value BLOB NOT NULL, module_name TEXT NOT NULL, UNIQUE(path) on conflict replace, UNIQUE(module_name) on conflict replace)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ef_imports (module_name TEXT references externs(module_name) ON DELETE CASCADE, imported_module TEXT, import_type TEXT, imported_as TEXT, value BLOB)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS available_srcs (path TEXT PRIMARY KEY NOT NULL, UNIQUE(path) on conflict replace)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS export_environments (path TEXT PRIMARY KEY NOT NULL, hash INT NOT NULL, value BLOB NOT NULL, UNIQUE(path) on conflict replace)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS environments (path TEXT PRIMARY KEY NOT NULL, hash INT NOT NULL, value BLOB NOT NULL, UNIQUE(path) on conflict replace)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS value_operators (module_name TEXT references ast_modules(module_name) ON DELETE CASCADE, op_name TEXT, alias_module_name TEXT, alias TEXT, associativity TEXT, precedence INTEGER, UNIQUE(module_name, op_name) on conflict replace)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS type_operators (module_name TEXT references ast_modules(module_name) ON DELETE CASCADE, op_name TEXT, alias_module_name TEXT, alias TEXT, associativity TEXT, precedence INTEGER, UNIQUE(module_name, op_name) on conflict replace)"
  initEnvTables conn
  addDbIndexes conn

addDbIndexes :: Connection -> IO ()
addDbIndexes conn = do
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_declarations_module_name ON ast_declarations (module_name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_declarations_name ON ast_declarations (name)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_declarations_name_type ON ast_declarations (name_type)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS ast_declarations_ctr_type ON ast_declarations (ctr_type)"
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
  dropEnvTables conn

indexExportedEnv :: (MonadIO m) => P.ModuleName -> E.Environment -> Maybe [DeclarationRef] -> Connection -> m ()
indexExportedEnv moduleName env refs conn = do 
 liftIO $ labelError "indexExportedEnv" do
  deleteModuleEnv
  envFromModule E.names & filter nameExported & mapConcurrently_ (uncurry $ insertEnvValue conn)
  envFromModule E.types & filter typeOrClassExported & mapConcurrently_ (uncurry $ insertType conn)
  envFromModule E.dataConstructors & filter dataConstructorExported & mapConcurrently_ (uncurry $ insertDataConstructor conn)
  envFromModule E.typeSynonyms & filter typeExported & mapConcurrently_ (uncurry $ insertTypeSynonym conn)
  envFromModule E.typeClasses & filter typeClassExported & mapConcurrently_ (uncurry $ insertTypeClass conn)
  dicts
    & filter ((== Just moduleName) . P.getQual . tcdValue)
    & mapConcurrently_ (insertNamedDict conn)
  where
    envFromModule :: (E.Environment -> Map.Map (Qualified k) v) -> [(Qualified k, v)]
    envFromModule f = f env & Map.toList & filter ((== Just moduleName) . P.getQual . fst)

    dicts :: [NamedDict]
    dicts =
      E.typeClassDictionaries env
        & Map.elems
          >>= Map.elems
          >>= Map.elems
          >>= toList
        <&> localToQualified

    localToQualified :: NamedDict -> NamedDict
    localToQualified dict =
      if P.isQualified (tcdValue dict)
        then dict
        else dict {tcdValue = P.Qualified (P.ByModuleName moduleName) (P.disqualify $ tcdValue dict)}

    deleteModuleEnv = do
      SQL.execute conn "DELETE FROM env_values WHERE module_name = ?" (SQL.Only moduleName)
      SQL.execute conn "DELETE FROM env_types WHERE module_name = ?" (SQL.Only moduleName)
      SQL.execute conn "DELETE FROM env_data_constructors WHERE module_name = ?" (SQL.Only moduleName)
      SQL.execute conn "DELETE FROM env_type_synonyms WHERE module_name = ?" (SQL.Only moduleName)
      SQL.execute conn "DELETE FROM env_type_classes WHERE module_name = ?" (SQL.Only moduleName)

    refMatch :: (Qualified a -> DeclarationRef -> Bool) -> (Qualified a, b) -> Bool
    refMatch f (k, _) = maybe True (any (f k)) refs

    nameExported = refMatch \k -> \case
      P.ValueRef _ ident -> ident == P.disqualify k
      _ -> False

    typeClassExported = refMatch \k -> \case
      P.TypeClassRef _ className -> className == P.disqualify k
      _ -> False

    typeOrClassExported :: (Qualified (P.ProperName 'P.TypeName), b) -> Bool
    typeOrClassExported kv = typeExported kv || typeClassExported (first (fmap P.coerceProperName) kv)

    typeExported = refMatch \k -> \case
      P.TypeRef _ typeName _ -> typeName == P.disqualify k
      _ -> False

    dataConstructorExported = refMatch \k -> \case
      P.TypeRef _ _ ctrs -> maybe False (elem (P.disqualify k)) ctrs
      _ -> False

type EnvValue = (P.SourceType, P.NameKind, P.NameVisibility)

insertEnvValue :: Connection -> P.Qualified P.Ident -> EnvValue -> IO ()
insertEnvValue conn ident val = labelError "insertEnvValue" do
  SQL.execute
    conn
    "INSERT OR REPLACE INTO env_values (module_name, ident, source_type, name_kind, name_visibility) VALUES (?, ?, ?, ?, ?)"
    (toDbQualifer ident :. val)

type EnvType = (P.SourceType, P.TypeKind)

insertType :: Connection -> P.Qualified (P.ProperName 'P.TypeName) -> EnvType -> IO ()
insertType conn ident val = labelError "insertType" do
  SQL.execute
    conn
    "INSERT OR REPLACE INTO env_types (module_name, type_name, source_type, type_kind) VALUES (?, ?, ?, ?)"
    (toDbQualifer ident :. val)

insertDataConstructor :: Connection -> P.Qualified (P.ProperName 'P.ConstructorName) -> (P.DataDeclType, P.ProperName 'P.TypeName, P.SourceType, [P.Ident]) -> IO ()
insertDataConstructor conn ident (ddt, ty, st, idents) = labelError "insertDataConstructor" do
  SQL.execute
    conn
    "INSERT OR REPLACE INTO env_data_constructors (module_name, constructor_name, data_decl_type, type_name, source_type, idents) VALUES (?, ?, ?, ?, ?, ?)"
    (toDbQualifer ident :. (ddt, ty, st, serialise idents))

insertTypeSynonym :: Connection -> P.Qualified (P.ProperName 'P.TypeName) -> ([(Text, Maybe P.SourceType)], P.SourceType) -> IO ()
insertTypeSynonym conn ident (idents, st) = labelError "insertTypeSynonym" do
  SQL.execute
    conn
    "INSERT OR REPLACE INTO env_type_synonyms (module_name, type_name, idents, source_type) VALUES (?, ?, ?, ?)"
    (toDbQualifer ident :. (serialise idents, st))

insertTypeClass :: Connection -> P.Qualified (P.ProperName 'P.ClassName) -> P.TypeClassData -> IO ()
insertTypeClass conn ident tcd = labelError "insertTypeClass" do
  SQL.execute
    conn
    "INSERT OR REPLACE INTO env_type_classes (module_name, class_name, class) VALUES (?, ?, ?)"
    (toDbQualifer ident :. SQL.Only tcd)

insertNamedDict :: Connection -> NamedDict -> IO ()
insertNamedDict conn dict = labelError "insertNamedDict" do
  SQL.execute
    conn
    "INSERT OR REPLACE INTO env_type_class_instances (module_name, instance_name, class_module, class_name, dict) VALUES (?, ?, ?, ?, ?)"
    (toDbQualifer (tcdValue dict) :. (clasMod, className, serialise dict))

  where 
    (clasMod, className) = toDbQualifer (tcdClassName dict)
    

initEnvTables :: Connection -> IO ()
initEnvTables conn = do
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS env_values (module_name TEXT, ident TEXT, source_type BLOB, name_kind TEXT, name_visibility TEXT, debug TEXT)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS env_types (module_name TEXT, type_name TEXT, source_type BLOB, type_kind TEXT, debug TEXT)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS env_data_constructors (module_name TEXT, constructor_name TEXT, data_decl_type TEXT, type_name TEXT, source_type BLOB, idents BLOB, debug TEXT)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS env_type_synonyms (module_name TEXT, type_name TEXT, idents BLOB, source_type BLOB, debug TEXT)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS env_type_classes (module_name TEXT, class_name TEXT, class BLOB, debug TEXT)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS env_type_class_instances (module_name TEXT, instance_name TEXT, class_module TEXT, class_name TEXT, dict BLOB, debug TEXT)"
  addEnvIndexes conn

addEnvIndexes :: Connection -> IO ()
addEnvIndexes conn = do
  SQL.execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS env_values_idx ON env_values(module_name, ident)"
  SQL.execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS env_types_idx ON env_types(module_name, type_name)"
  SQL.execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS env_data_constructors_idx ON env_data_constructors(module_name, constructor_name)"
  SQL.execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS env_type_synonyms_idx ON env_type_synonyms(module_name, type_name)"
  SQL.execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS env_type_classes_idx ON env_type_classes(module_name, class_name)"
  SQL.execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS env_type_class_instances_idx ON env_type_class_instances(module_name, instance_name)"

dropEnvTables :: Connection -> IO ()
dropEnvTables conn = do
  SQL.execute_ conn "DROP TABLE IF EXISTS env_values"
  SQL.execute_ conn "DROP TABLE IF EXISTS env_types"
  SQL.execute_ conn "DROP TABLE IF EXISTS env_data_constructors"
  SQL.execute_ conn "DROP TABLE IF EXISTS env_type_synonyms"
  SQL.execute_ conn "DROP TABLE IF EXISTS env_type_classes"


labelError :: Text -> IO a -> IO a 
labelError label action = catch action \(e :: SomeException) -> do 
  putErrLn $ "Error: " <> label <> ": " <> show e
  throwIO e