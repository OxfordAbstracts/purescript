{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE InstanceSigs #-}

module Language.PureScript.Make.Index.Select where

import Codec.Serialise (deserialise)
import Control.Arrow ((>>>))
import Control.Concurrent.Async.Lifted (forConcurrently, forConcurrently_, mapConcurrently, mapConcurrently_)
import Control.Lens (Field1 (_1), Field2 (_2), Field3 (_3), view)
import Control.Monad.Writer (MonadWriter (tell), Writer, execWriter, WriterT)
import Control.Monad.Writer.Strict qualified as Strict
import Data.Aeson qualified as A
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Language.PureScript.AST.Declarations (ImportDeclarationType)
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.AST.Operators qualified as P
import Language.PureScript.AST.Traversals (accumTypes)
import Language.PureScript.AST.Traversals qualified as P
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (TypeClassData (typeClassSuperclasses))
import Language.PureScript.Environment qualified as E
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs (ExternsFixity (..), ExternsTypeFixity (..))
import Language.PureScript.Linter.Imports qualified as P
import Language.PureScript.Names (coerceProperName)
import Language.PureScript.Names qualified as P
import Language.PureScript.Sugar.BindingGroups (usedTypeNames)
import Language.PureScript.Sugar.Names (Exports (exportedValueOps))
import Language.PureScript.Sugar.Names.Env qualified as P
-- import Language.PureScript.TypeChecker.Monad qualified as P
import Language.PureScript.TypeClassDictionaries (NamedDict)
import Language.PureScript.TypeClassDictionaries qualified as P
import Language.PureScript.Types (Constraint (constraintClass))
import Language.PureScript.Types qualified as P
import Protolude hiding (moduleName)
import Protolude.Partial (fromJust)
import Control.Monad.Supply (SupplyT)
import Control.Monad.Supply.Class (MonadSupply (fresh, peek))
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)


selectFixitiesFromModuleImportsAndDecls :: Connection -> P.Env -> P.Module -> IO ([(P.ModuleName, [ExternsFixity])], [(P.ModuleName, [ExternsTypeFixity])])
selectFixitiesFromModuleImportsAndDecls conn env module' = do
  (fixitiesFromImports, typeFixitiesFromImports) <- selectFixitiesFromModuleImports conn env module'
  let (fixitiesFromDecls, typeFixitiesFromDecls) = getModuleFixities module'
  pure ((P.getModuleName module', fixitiesFromDecls) : fixitiesFromImports, (P.getModuleName module', typeFixitiesFromDecls) : typeFixitiesFromImports)

getModuleFixities :: P.Module -> ([ExternsFixity], [ExternsTypeFixity])
getModuleFixities (P.Module _ _ _ decls _) = (externsFixitiesInModule, externsTypeFixitiesInModule)
  where
    externsFixitiesInModule :: [ExternsFixity]
    externsFixitiesInModule =
      fixitiesInModule <&> \(P.ValueFixity (P.Fixity assoc prec) ident opName) ->
        ExternsFixity assoc prec opName ident

    externsTypeFixitiesInModule :: [ExternsTypeFixity]
    externsTypeFixitiesInModule =
      typeFixitiesInModule <&> \(P.TypeFixity (P.Fixity assoc prec) ident opName) ->
        ExternsTypeFixity assoc prec opName ident

    (fixitiesInModule, typeFixitiesInModule) =
      partitionEithers $
        decls >>= \case
          P.FixityDeclaration _ fixity -> [fixity]
          _ -> []

selectFixitiesFromModuleImports :: Connection -> P.Env -> P.Module -> IO ([(P.ModuleName, [ExternsFixity])], [(P.ModuleName, [ExternsTypeFixity])])
selectFixitiesFromModuleImports conn env (P.Module _ _ _modName decls _refs) = do
  valueOps <- onImports selectImportValueFixities
  -- when (_modName == P.ModuleName "Data.NonEmpty") do 
  -- putErrText $ show _modName
  -- putErrText $ "valueOps: " <> show valueOps
  typeOps <- onImports selectImportTypeFixities
  pure (valueOps, typeOps)
  where
    onImports :: Ord a =>
      (Connection -> P.Env -> P.ModuleName -> ImportDeclarationType -> IO [(P.ModuleName, a)]) ->
      IO [(P.ModuleName, [a])]
    onImports fn = groupByModule . join . catMaybes <$> forConcurrently decls (whenImportDecl (fn conn env))

    whenImportDecl :: (P.ModuleName -> ImportDeclarationType -> IO [(P.ModuleName, a)]) -> P.Declaration -> IO (Maybe [(P.ModuleName, a)])
    whenImportDecl f = \case
      P.ImportDeclaration _ mn' idt importedAs -> Just  <$> f mn' idt
        where 
          addImportedAs (mn'', a) = (fromMaybe mn'' importedAs, a)
      _ -> pure Nothing

    groupByModule :: Ord a => [(P.ModuleName, a)] -> [(P.ModuleName, [a])]
    groupByModule = Map.toList . fmap ordNub . Map.fromListWith (<>) . fmap (fmap pure)

selectImportValueFixities :: Connection -> P.Env -> P.ModuleName -> ImportDeclarationType -> IO [(P.ModuleName, ExternsFixity)]
selectImportValueFixities conn env modName = \case
  P.Implicit -> selectValueFixitiesFromExports conn exports
  P.Explicit refs -> selectValueFixitiesFromExports conn $ Map.filterWithKey (inRefs refs) exports
  P.Hiding refs -> selectValueFixitiesFromExports conn $ Map.filterWithKey (fmap not . inRefs refs) exports
  where
    exports = exportedValueOps $ lookupExports modName env
    inRefs refs opName _ = opName `elem` opRefs
      where
        opRefs = refsValueOps env refs

lookupExports :: P.ModuleName -> P.Env -> Exports
lookupExports modName env = maybe P.nullExports (view _3) (Map.lookup modName env)

lookupImports :: P.ModuleName -> P.Env -> P.Imports
lookupImports modName env = view _2 $ fromJust $ Map.lookup modName env

refsValueOps :: P.Env -> [P.DeclarationRef] -> [P.OpName 'P.ValueOpName]
refsValueOps env = (=<<) (refValueOp env)

refValueOp :: P.Env -> P.DeclarationRef -> [P.OpName 'P.ValueOpName]
refValueOp env = \case
  P.ValueOpRef _ ident -> pure ident
  P.ReExportRef _ _ ref -> refValueOp env ref
  _ -> []

selectValueFixitiesFromExports :: Connection -> Map (P.OpName 'P.ValueOpName) P.ExportSource -> IO [(P.ModuleName, ExternsFixity)]
selectValueFixitiesFromExports conn = fmap catMaybes . mapConcurrently select . Map.toList
  where
    select (opName, P.ExportSource {..}) = fmap (exportSourceDefinedIn,) <$> selectImportValueFixity conn exportSourceDefinedIn opName

selectImportValueFixity :: Connection -> P.ModuleName -> P.OpName 'P.ValueOpName -> IO (Maybe ExternsFixity)
selectImportValueFixity conn modName opName = do
  SQL.query
    conn
    "SELECT associativity, precedence, op_name, alias_module_name, alias FROM value_operators WHERE module_name = ? AND op_name = ?"
    (modName, opName)
    <&> head

selectImportTypeFixities :: Connection -> P.Env -> P.ModuleName -> ImportDeclarationType -> IO [(P.ModuleName, ExternsTypeFixity)]
selectImportTypeFixities conn env modName = \case
  P.Implicit -> selectTypeFixitiesFromExports conn exports
  P.Explicit refs -> selectTypeFixitiesFromExports conn $ Map.filterWithKey (inRefs refs) exports
  P.Hiding refs -> selectTypeFixitiesFromExports conn $ Map.filterWithKey (fmap not . inRefs refs) exports
  where
    exports = P.exportedTypeOps $ view _3 $ fromJust $ Map.lookup modName env
    inRefs refs opName _ = opName `elem` opRefs
      where
        opRefs = refsTypeOps refs

refsTypeOps :: [P.DeclarationRef] -> [P.OpName 'P.TypeOpName]
refsTypeOps = mapMaybe refTypeOp

refTypeOp :: P.DeclarationRef -> Maybe (P.OpName 'P.TypeOpName)
refTypeOp = \case
  P.TypeOpRef _ ident -> Just ident
  P.ReExportRef _ _ ref -> refTypeOp ref
  _ -> Nothing

selectTypeFixitiesFromExports :: Connection -> Map (P.OpName 'P.TypeOpName) P.ExportSource -> IO [(P.ModuleName, ExternsTypeFixity)]
selectTypeFixitiesFromExports conn = fmap catMaybes . mapConcurrently select . Map.toList
  where
    select (opName, P.ExportSource {..}) = fmap (exportSourceDefinedIn,) <$> selectImportTypeFixity conn exportSourceDefinedIn opName

selectImportTypeFixity :: Connection -> P.ModuleName -> P.OpName 'P.TypeOpName -> IO (Maybe ExternsTypeFixity)
selectImportTypeFixity conn modName opName = do
  SQL.query
    conn
    "SELECT associativity, precedence, op_name, alias_module_name, alias FROM type_operators WHERE module_name = ? AND op_name = ?"
    (modName, opName)
    <&> head

type ClassDict =
  Map.Map
    P.QualifiedBy
    ( Map.Map
        (P.Qualified (P.ProperName 'P.ClassName))
        (Map.Map (P.Qualified P.Ident) (NEL.NonEmpty NamedDict))
    )

getTypesToImportFromEnv :: P.Environment -> Set (P.Qualified ToImport)
getTypesToImportFromEnv env =
  nameImports
    <> typeImports
    <> ctrImports
    <> synonymImports
    <> dictImports
    <> classImports
  where
    nameImports =
      E.names env
        & Map.elems
        <&> (typesToImport . view _1)
        & Set.unions

    typeImports =
      E.types env
        & Map.elems
        <&> (typesToImport . view _1)
        & Set.unions

    ctrImports =
      E.dataConstructors env
        & Map.elems
        <&> (typesToImport . view _3)
        & Set.unions

    synonymImports =
      E.typeSynonyms env
        & Map.elems
        <&> (typesToImport . view _2)
        & Set.unions

    dictImports =
      E.typeClassDictionaries env
        & Map.elems
          >>= Map.elems
          >>= (fmap (Set.unions . fmap namedDictImports) . Map.elems)
        & Set.unions

    classImports =
      E.typeClasses env
        & Map.elems
        <&> typeClassImports
        & Set.unions

namedDictImports :: NamedDict -> Set (P.Qualified ToImport)
namedDictImports dict = P.tcdDependencies dict & maybe Set.empty (Set.unions . fmap (Set.unions . fmap typesToImport . P.constraintArgs))

typeClassImports :: P.TypeClassData -> Set (P.Qualified ToImport)
typeClassImports tcd =
  P.typeClassSuperclasses tcd
    <&> constraintImports
    & Set.unions

typeClassDataTypes :: P.TypeClassData -> [P.SourceType]
typeClassDataTypes tcd = P.typeClassSuperclasses tcd >>= P.constraintArgs

getUsedNames :: P.Declaration -> Set (P.Qualified ToImport)
getUsedNames d = getUsedValueNames d <> getDeclTypesToImport d

getUsedValueNames :: P.Declaration -> Set (P.Qualified ToImport)
getUsedValueNames = execWriter . handleDecl
  where
    (handleDecl, _, _) = P.everywhereOnValuesM onDecl onExpr pure

    onDecl :: P.Declaration -> Writer (Set (P.Qualified ToImport)) P.Declaration
    onDecl d = do
      case d of
        P.TypeInstanceDeclaration _ _ _ _ _ deps cl _ _ ->
          tell $
            Set.fromList [fmap TiClass cl, TiType . P.coerceProperName <$> cl] <> Set.unions (constraintImports <$> deps)
        _ -> pure ()
      pure d

    onExpr :: P.Expr -> Writer (Set (P.Qualified ToImport)) P.Expr
    onExpr e = do
      case e of
        P.Var _ qn -> tell $ Set.singleton $ fmap TiIdent qn
        P.Constructor _ qn -> tell $ Set.singleton $ fmap TiDctor qn
        P.DeferredDictionary cn@(P.Qualified qb cn') _types ->
          tell $
            Set.fromList
              [ fmap TiClass cn,
                TiType . P.coerceProperName <$> cn,
                P.Qualified qb $ TiDeferredDictionary cn'
              ]
        P.DerivedInstancePlaceholder cn _ ->
          tell $
            Set.fromList
              [ fmap TiClass cn,
                TiType . P.coerceProperName <$> cn
              ]
        _ -> pure ()
      pure e

getDeclTypesToImport :: P.Declaration -> Set (P.Qualified ToImport)
getDeclTypesToImport = declTypeNames
  where
    (declTypeNames, _, _, _, _) = P.accumTypes $ P.everythingOnTypes (<>) \case
      P.TypeConstructor _ tyName -> Set.singleton $ fmap TiType tyName
      P.ConstrainedType _ c _ -> constraintImports c
        where
          (P.Qualified qb cl) = P.constraintClass c
      _ -> Set.empty

constraintImports :: P.SourceConstraint -> Set (P.Qualified ToImport)
constraintImports c =
  Set.fromList
    [ TiClass <$> P.constraintClass c,
      TiType . P.coerceProperName <$> P.constraintClass c,
      P.Qualified qb $ TiDeferredDictionary cl
    ]
  where
    (P.Qualified qb cl) = P.constraintClass c

typesToImport :: P.SourceType -> Set (P.Qualified ToImport)
typesToImport = P.everythingOnTypes (<>) \case
  P.TypeConstructor _ tyName -> Set.singleton $ fmap TiType tyName
  P.ConstrainedType _ c _ ->
    Set.fromList
      [ TiClass <$> P.constraintClass c,
        TiType . P.coerceProperName <$> P.constraintClass c,
        P.Qualified qb $ TiDeferredDictionary cl
      ]
    where
      (P.Qualified qb cl) = P.constraintClass c
  _ -> Set.empty

data ToImport
  = TiIdent P.Ident
  | TiType (P.ProperName 'P.TypeName)
  | TiDctor (P.ProperName 'P.ConstructorName)
  | TiClass (P.ProperName 'P.ClassName)
  | TiDeferredDictionary (P.ProperName 'P.ClassName)
  deriving (Show, Eq, Ord)

selectModuleExports :: Connection -> P.ModuleName -> IO [P.DeclarationRef]
selectModuleExports conn modName = do
  SQL.query
    conn
    "SELECT value FROM exports WHERE module_name = ?"
    (SQL.Only modName)
    <&> fmap SQL.fromOnly

insertExports :: Connection -> P.ModuleName -> Maybe [P.DeclarationRef] -> IO ()
insertExports conn modName = \case
  Nothing -> internalError "selectEnvFromImports called before desguaring module"
  Just refs -> forConcurrently_ refs (insertExport conn modName)

insertExport :: Connection -> P.ModuleName -> P.DeclarationRef -> IO ()
insertExport conn modName ref =
  SQL.execute
    conn
    "INSERT INTO exports (module_name, ident, value) VALUES (?, ?, ?)"
    (modName, (show $ P.declRefName ref) :: Text, ref)

selectEnvValue :: Connection -> P.Qualified P.Ident -> IO (Maybe (P.SourceType, P.NameKind, P.NameVisibility))
selectEnvValue conn ident = do
  SQL.query
    conn
    "SELECT source_type, name_kind, name_visibility FROM env_values WHERE module_name = ? AND ident = ?"
    (toDbQualifer ident)
    <&> head

selectModuleEnvValues :: Connection -> P.ModuleName -> IO [(P.Qualified P.Ident, (P.SourceType, P.NameKind, P.NameVisibility))]
selectModuleEnvValues conn moduleName' = do
  SQL.query
    conn
    "SELECT ident, source_type, name_kind, name_visibility FROM env_values WHERE module_name = ?"
    (SQL.Only moduleName')
    <&> fmap (\(ident, st, nk, nv) -> (P.Qualified (P.ByModuleName moduleName') ident, (st, nk, nv)))

selectType :: Connection -> P.Qualified (P.ProperName 'P.TypeName) -> IO (Maybe (P.SourceType, P.TypeKind))
selectType conn ident = case Map.lookup ident P.allPrimTypes of
  Just a -> pure $ Just a
  Nothing ->
    SQL.query
      conn
      "SELECT source_type, type_kind FROM env_types WHERE module_name = ? AND type_name = ?"
      (modName, ty_name)
      <&> head
  where
    (modName, ty_name) = toDbQualifer ident

selectType' :: Connection -> P.ModuleName -> P.ProperName 'P.TypeName -> IO (Maybe (P.SourceType, P.TypeKind))
selectType' conn nMame ident = selectType conn (P.Qualified (P.ByModuleName nMame) ident)

selectModuleEnvTypes :: Connection -> P.ModuleName -> IO [(P.Qualified (P.ProperName 'P.TypeName), (P.SourceType, P.TypeKind))]
selectModuleEnvTypes conn moduleName' = do
  SQL.query
    conn
    "SELECT type_name, source_type, type_kind FROM env_types WHERE module_name = ?"
    (SQL.Only moduleName')
    <&> fmap (\(ty, st, tk) -> (P.Qualified (P.ByModuleName moduleName') ty, (st, tk)))

selectDataConstructor :: Connection -> P.Qualified (P.ProperName 'P.ConstructorName) -> IO (Maybe (P.DataDeclType, P.ProperName 'P.TypeName, P.SourceType, [P.Ident]))
selectDataConstructor conn ident = do
  SQL.query
    conn
    "SELECT data_decl_type, type_name, source_type, idents FROM env_data_constructors WHERE module_name = ? AND constructor_name = ?"
    (toDbQualifer ident)
    <&> (head >>> fmap deserialiseIdents)
  where
    deserialiseIdents (ddt, ty, st, idents) = (ddt, ty, st, deserialise idents)

selectTypeDataConstructors :: Connection -> P.Qualified (P.ProperName 'P.TypeName) -> IO [(P.Qualified (P.ProperName 'P.ConstructorName), (P.DataDeclType, P.ProperName 'P.TypeName, P.SourceType, [P.Ident]))]
selectTypeDataConstructors conn ident = do
  SQL.query
    conn
    "SELECT constructor_name, data_decl_type, type_name, source_type, idents FROM env_data_constructors WHERE module_name = ? AND type_name = ?"
    (toDbQualifer ident)
    <&> fmap (\(ctr, ddt, ty, st, idents) -> (P.Qualified (P.ByModuleName moduleName') ctr, (ddt, ty, st, deserialise idents)))
  where
    moduleName' = fromJust $ P.getQual ident

selectModuleDataConstructors :: Connection -> P.ModuleName -> IO [(P.Qualified (P.ProperName 'P.ConstructorName), (P.DataDeclType, P.ProperName 'P.TypeName, P.SourceType, [P.Ident]))]
selectModuleDataConstructors conn moduleName' = do
  SQL.query
    conn
    "SELECT constructor_name, data_decl_type, type_name, source_type, idents FROM env_data_constructors WHERE module_name = ?"
    (SQL.Only moduleName')
    <&> fmap (\(ctr, ddt, ty, st, idents) -> (P.Qualified (P.ByModuleName moduleName') ctr, (ddt, ty, st, deserialise idents)))

selectTypeSynonym :: Connection -> P.Qualified (P.ProperName 'P.TypeName) -> IO (Maybe ([(Text, Maybe P.SourceType)], P.SourceType))
selectTypeSynonym conn ident = do
  SQL.query
    conn
    "SELECT idents, source_type FROM env_type_synonyms WHERE module_name = ? AND type_name = ?"
    (toDbQualifer ident)
    <&> (head >>> fmap deserialiseIdents)
  where
    deserialiseIdents (idents, st) = (deserialise idents, st)

selectTypeSynonym' :: Connection -> P.ModuleName -> P.ProperName 'P.TypeName -> IO (Maybe ([(Text, Maybe P.SourceType)], P.SourceType))
selectTypeSynonym' conn nMame ident = selectTypeSynonym conn (P.Qualified (P.ByModuleName nMame) ident)

selectModuleTypeSynonyms :: Connection -> P.ModuleName -> IO [(P.Qualified (P.ProperName 'P.TypeName), ([(Text, Maybe P.SourceType)], P.SourceType))]
selectModuleTypeSynonyms conn moduleName' = do
  SQL.query
    conn
    "SELECT type_name, idents, source_type FROM env_type_synonyms WHERE module_name = ?"
    (SQL.Only moduleName')
    <&> fmap (\(ty, idents, st) -> (P.Qualified (P.ByModuleName moduleName') ty, (deserialise idents, st)))

selectTypeClass :: Connection -> P.ModuleName -> P.ProperName 'P.ClassName -> IO (Maybe P.TypeClassData)
selectTypeClass conn modName className =
  case Map.lookup (P.Qualified (P.ByModuleName modName) className) P.allPrimClasses of
    Just a -> pure $ Just a
    Nothing ->
      SQL.query
        conn
        "SELECT class FROM env_type_classes WHERE module_name = ? AND class_name = ?"
        (modName, className)
        <&> (fmap SQL.fromOnly . head)

selectTypeClass' :: Connection -> P.Qualified (P.ProperName 'P.ClassName) -> IO (Maybe P.TypeClassData)
selectTypeClass' conn = \case 
   P.Qualified (P.ByModuleName modName) className -> selectTypeClass conn modName className
   _ -> pure Nothing 

selectModuleTypeClasses :: Connection -> P.ModuleName -> IO [(P.Qualified (P.ProperName 'P.ClassName), P.TypeClassData)]
selectModuleTypeClasses conn moduleName' = do
  SQL.query
    conn
    "SELECT class_name, class FROM env_type_classes WHERE module_name = ?"
    (SQL.Only moduleName')
    <&> fmap (first (P.Qualified (P.ByModuleName moduleName')))

selectAllClassInstances :: 
  Connection -> 
  IO [NamedDict]
selectAllClassInstances conn  = do
  SQL.query_
    conn
    "SELECT dict FROM env_type_class_instances"
    <&> (fmap (SQL.fromOnly >>> deserialise))

selectClassInstances ::
  Connection ->
  P.Qualified (P.ProperName 'P.ClassName) ->
  [P.Type ()] ->
  IO [NamedDict]
selectClassInstances conn classNameQual types = do
  SQL.query
    conn
    "SELECT dict FROM env_type_class_instances WHERE module_name = ? AND class_name = ? AND types = ?"
    (modName, className, A.encode types)
    <&> (fmap (SQL.fromOnly >>> deserialise))
  where
    (modName, className) = toDbQualifer classNameQual

selectModuleClassInstances :: Connection -> P.ModuleName -> IO [NamedDict]
selectModuleClassInstances conn moduleName' = do
  SQL.query
    conn
    "SELECT dict FROM env_type_class_instances WHERE module_name = ?"
    (SQL.Only moduleName')
    <&> fmap (SQL.fromOnly >>> deserialise)

selectClassInstanceByIdents ::
  Connection ->
  P.Qualified (P.ProperName 'P.ClassName) ->
  [P.Ident] ->
  IO (Maybe NamedDict)
selectClassInstanceByIdents conn classNameQual idents = do
  SQL.query
    conn
    "SELECT dict FROM env_type_class_instances WHERE class_module = ? AND class_name = ? AND idents = ?"
    (modName, className, A.encode idents)
    <&> (head >>> fmap (SQL.fromOnly >>> deserialise))
  where
    (modName, className) = toDbQualifer classNameQual

-- TODO: Select specific instances instead of all
selectClassInstancesByClassName ::
  Connection ->
  P.Qualified (P.ProperName 'P.ClassName) ->
  IO [NamedDict]
selectClassInstancesByClassName conn classNameQual = do
  SQL.query
    conn
    "SELECT dict FROM env_type_class_instances WHERE class_module = ? AND class_name = ?"
    (modName, className)
    <&> fmap (SQL.fromOnly >>> deserialise)
  where
    (modName, className) = toDbQualifer classNameQual

selectValueOperatorAlias :: Connection -> P.ModuleName -> P.OpName 'P.ValueOpName -> IO (Maybe (P.ModuleName, Text))
selectValueOperatorAlias conn modName opName = do
  SQL.query
    conn
    "SELECT alias_module_name, alias FROM value_operators WHERE module_name = ? AND op_name = ?"
    (modName, P.runOpName opName)
    <&> head

selectTypeOperatorAlias :: Connection -> P.ModuleName -> P.OpName 'P.TypeOpName -> IO (Maybe (P.ModuleName, P.ProperName 'P.TypeName))
selectTypeOperatorAlias conn modName opName = do
  SQL.query
    conn
    "SELECT alias_module_name, alias FROM type_operators WHERE module_name = ? AND op_name = ?"
    (modName, P.runOpName opName)
    <&> head

selectImportedAs' :: Connection -> P.ModuleName -> P.ModuleName -> IO P.ModuleName
selectImportedAs' conn modName importedModName = fromMaybe importedModName <$> selectImportedAs conn modName importedModName

selectImportedAs :: Connection -> P.ModuleName -> P.ModuleName -> IO (Maybe P.ModuleName)
selectImportedAs conn modName importedModName = do
  SQL.query
    conn
    "SELECT imported_as FROM imports WHERE module_name = ? AND imported_module_name = ?"
    (modName, importedModName)
    <&> (head >>> fmap SQL.fromOnly >>> join)

type DbQualifer a = (P.ModuleName, a)

toDbQualifer :: P.Qualified a -> DbQualifer a
toDbQualifer (P.Qualified (P.ByModuleName mn) a) = (mn, a)
toDbQualifer (P.Qualified (P.BySourcePos _) _) = internalError "toDbQualifer called with BySourcePos"

fromJustWithErr :: (HasCallStack) => (Show e) => e -> Maybe a -> a
fromJustWithErr err = \case
  Just a -> a
  Nothing -> internalError $ "fromJustWithErr: " <> show err

insertImports :: Connection -> P.ModuleName -> [P.Declaration] -> IO ()
insertImports conn mn = mapConcurrently_ (insertImport conn mn)

insertImport :: Connection -> P.ModuleName -> P.Declaration -> IO ()
insertImport conn mn = \case
  P.ImportDeclaration _ importedModuleName _ importedAs -> do
    SQL.execute
      conn
      "INSERT INTO imports (module_name, imported_module, imported_as) VALUES (?, ?, ?)"
      (mn, importedModuleName, importedAs)
  _ -> pure ()

deleteModuleEnvImpl :: P.ModuleName -> Connection -> IO ()
deleteModuleEnvImpl moduleName conn = do
  SQL.execute conn "DELETE FROM env_values WHERE module_name = ?" (SQL.Only moduleName)
  SQL.execute conn "DELETE FROM env_types WHERE module_name = ?" (SQL.Only moduleName)
  SQL.execute conn "DELETE FROM env_data_constructors WHERE module_name = ?" (SQL.Only moduleName)
  SQL.execute conn "DELETE FROM env_type_synonyms WHERE module_name = ?" (SQL.Only moduleName)
  SQL.execute conn "DELETE FROM env_type_classes WHERE module_name = ?" (SQL.Only moduleName)

getEnvConstraints :: E.Environment -> [P.SourceConstraint]
getEnvConstraints env =
  E.names env & Map.elems >>= typeConstraints . view _1

typeConstraints :: P.Type a -> [P.Constraint a]
typeConstraints = P.everythingOnTypes (<>) \case
  P.ConstrainedType _ c _ -> [c]
  _ -> []

pipe :: [a -> a] -> a -> a
pipe = foldl' (>>>) identity

updateConcurrently :: IO (a -> b) -> IO (b -> c) -> IO (a -> c)
updateConcurrently a b = do
  f <- a
  g <- b
  pure $ f >>> g

class GetEnv m where 
  getName :: P.Qualified P.Ident -> m (Maybe (P.SourceType, P.NameKind, P.NameVisibility))
  getType :: P.Qualified (P.ProperName 'P.TypeName) -> m (Maybe (P.SourceType, P.TypeKind))
  getDataConstructor :: P.Qualified (P.ProperName 'P.ConstructorName) -> m (Maybe (P.DataDeclType, P.ProperName 'P.TypeName, P.SourceType, [P.Ident]))
  getTypeSynonym :: P.Qualified (P.ProperName 'P.TypeName) -> m (Maybe ([(Text, Maybe P.SourceType)], P.SourceType))
  getTypeClass :: P.Qualified (P.ProperName 'P.ClassName) -> m (Maybe P.TypeClassData)
  getTypeClassDictionaries :: m [NamedDict]
  getTypeClassDictionary :: P.Qualified (P.ProperName 'P.ClassName) -> m [NamedDict]
  deleteModuleEnv :: P.ModuleName -> m ()


instance (Monad m, GetEnv m) => GetEnv (MaybeT m ) where 
  getName = lift . getName
  getType = lift . getType
  getDataConstructor = lift . getDataConstructor
  getTypeSynonym = lift . getTypeSynonym
  getTypeClass = lift . getTypeClass
  getTypeClassDictionaries = lift getTypeClassDictionaries
  getTypeClassDictionary = lift . getTypeClassDictionary
  deleteModuleEnv = lift . deleteModuleEnv
instance (Monad m, GetEnv m) => GetEnv (ExceptT e m ) where 
  getName = lift . getName
  getType = lift . getType
  getDataConstructor = lift . getDataConstructor
  getTypeSynonym = lift . getTypeSynonym
  getTypeClass = lift . getTypeClass
  getTypeClassDictionaries = lift getTypeClassDictionaries
  getTypeClassDictionary = lift . getTypeClassDictionary
  deleteModuleEnv = lift . deleteModuleEnv

instance (Monad m, Monoid w, GetEnv m) => GetEnv (WriterT w m ) where 
  getName = lift . getName
  getType = lift . getType
  getDataConstructor = lift . getDataConstructor
  getTypeSynonym = lift . getTypeSynonym
  getTypeClass = lift . getTypeClass
  getTypeClassDictionaries = lift getTypeClassDictionaries
  getTypeClassDictionary = lift . getTypeClassDictionary
  deleteModuleEnv = lift . deleteModuleEnv
instance (Monad m, Monoid w, GetEnv m) => GetEnv (Strict.WriterT w m ) where 
  getName = lift . getName
  getType = lift . getType
  getDataConstructor = lift . getDataConstructor
  getTypeSynonym = lift . getTypeSynonym
  getTypeClass = lift . getTypeClass
  getTypeClassDictionaries = lift getTypeClassDictionaries
  getTypeClassDictionary = lift . getTypeClassDictionary
  deleteModuleEnv = lift . deleteModuleEnv

newtype DbEnv m a = DbEnv (ReaderT Connection m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState s, MonadError e, MonadWriter w, MonadTrans)

instance MonadSupply m => MonadSupply (DbEnv m)


runDbEnv :: Connection -> DbEnv m a -> m a
runDbEnv conn (DbEnv m) = runReaderT m conn

instance (MonadIO m) => GetEnv (DbEnv m) where
  getName ident = DbEnv $ do
    conn <- ask
    liftIO $ selectEnvValue conn ident
  getType ty = DbEnv $ do
    conn <- ask
    liftIO $ selectType conn ty
  getDataConstructor ctr = DbEnv $ do
    conn <- ask
    liftIO $ selectDataConstructor conn ctr
  getTypeSynonym syn = DbEnv $ do
    conn <- ask
    liftIO $ selectTypeSynonym conn syn
  getTypeClass cls = DbEnv $ do
    conn <- ask
    liftIO $ selectTypeClass' conn cls
  getTypeClassDictionaries = DbEnv $ do
    conn <- ask
    liftIO $ selectAllClassInstances conn
  deleteModuleEnv modName = DbEnv $ do
    conn <- ask
    liftIO $ deleteModuleEnvImpl modName conn


  getTypeClassDictionary cls = DbEnv $ do
    conn <- ask
    liftIO $ selectClassInstancesByClassName conn cls


newtype WoGetEnv m a = WoGetEnv (m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState s, MonadError e, MonadWriter w)

runWoGetEnv :: WoGetEnv m a -> m a
runWoGetEnv (WoGetEnv m) = m

instance MonadSupply m => MonadSupply (WoGetEnv m) where 
  fresh = WoGetEnv fresh
  peek = WoGetEnv peek

instance Monad m => GetEnv (WoGetEnv m) where 
  getName _ = pure Nothing
  getType _ = pure Nothing
  getDataConstructor _ = pure Nothing
  getTypeSynonym _ = pure Nothing
  getTypeClass _ = pure Nothing
  getTypeClassDictionaries = pure []
  getTypeClassDictionary _ = pure []
  deleteModuleEnv _ = pure ()