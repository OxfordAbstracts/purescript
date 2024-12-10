{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Make.Index.Select where

import Codec.Serialise (deserialise)
import Control.Arrow ((>>>))
import Control.Concurrent.Async.Lifted (forConcurrently, forConcurrently_, mapConcurrently, mapConcurrently_)
import Control.Lens (Field1 (_1), Field3 (_3), view, Field2 (_2))
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
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (TypeClassData (typeClassSuperclasses))
import Language.PureScript.Environment qualified as E
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs (ExternsFixity (..), ExternsTypeFixity (..))
import Language.PureScript.Names (coerceProperName)
import Language.PureScript.Names qualified as P
import Language.PureScript.Sugar.Names (Exports (exportedValueOps))
import Language.PureScript.Sugar.Names.Env qualified as P
import Language.PureScript.TypeChecker.Monad qualified as P
import Language.PureScript.TypeClassDictionaries (NamedDict)
import Language.PureScript.Types (Constraint (constraintClass))
import Language.PureScript.Types qualified as P
import Protolude hiding (moduleName)
import Protolude.Partial (fromJust)

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
  valueOps <- join . catMaybes <$> forConcurrently decls (onImports (selectImportValueFixities conn env))
  typeOps <- catMaybes <$> forConcurrently decls (onImports (selectImportTypeFixities conn env))
  -- when (_modName == P.ModuleName "Data.EuclideanRing") do 
  --   putErrText $ T.intercalate "\n\n" $ fmap show imports
  --   putErrText $ T.intercalate "\n\n" $ fmap show valueOps

  pure (groupByModule valueOps, typeOps)
  where
    onImports :: (P.ModuleName -> ImportDeclarationType -> IO a) -> P.Declaration -> IO (Maybe a)
    onImports f = \case
      P.ImportDeclaration _ mn' idt _ -> Just <$> f mn' idt
      _ -> pure Nothing


groupByModule :: [(P.ModuleName, a)] -> [(P.ModuleName, [a])]
groupByModule = Map.toList . Map.fromListWith (<>) . fmap (fmap pure)

    -- addOr' = if _modName == P.ModuleName "Data.EuclideanRing" then (\ops -> (P.ModuleName "Nonsense", [orImport]) : ops) else identity
    -- addOr' = if _modName == P.ModuleName "Data.EuclideanRing" then (\ops -> (P.ModuleName "Data.HeytingAlgebra", [orImport]) : ops) else identity

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

lookupExports :: P.ModuleName ->  P.Env -> Exports
lookupExports modName env  = view _3 $ fromJust $ Map.lookup modName env

lookupImports :: P.ModuleName ->  P.Env -> P.Imports
lookupImports modName env  = view _2 $ fromJust $ Map.lookup modName env
      
refsValueOps ::  P.Env -> [P.DeclarationRef] -> [P.OpName 'P.ValueOpName]
refsValueOps env = (=<<) (refValueOp env)

refValueOp :: P.Env -> P.DeclarationRef -> [P.OpName 'P.ValueOpName]
refValueOp env = \case
  P.ValueOpRef _ ident -> pure ident
  P.ReExportRef _ _ ref -> refValueOp env ref
  -- P.ModuleRef _ m -> _  env $ exportedValueOps $ lookupExports m env
  _ -> []

selectValueFixitiesFromExports :: Connection -> Map (P.OpName 'P.ValueOpName) P.ExportSource -> IO [(P.ModuleName, ExternsFixity)]
selectValueFixitiesFromExports conn = fmap catMaybes . mapConcurrently select . Map.toList
  where
    select (opName, P.ExportSource{..}) =  fmap (exportSourceDefinedIn, ) <$> selectImportValueFixity conn  exportSourceDefinedIn opName

addOr :: [ExternsFixity] -> [ExternsFixity]
addOr ops =  ExternsFixity P.Infixr 2 (P.OpName "||") (P.Qualified (P.ByModuleName $ P.ModuleName "Data.HeytingAlgebra") $ Left $ P.Ident "disj") : ops

orImport :: ExternsFixity
orImport =  ExternsFixity P.Infixr 2 (P.OpName "||") (P.Qualified (P.ByModuleName $ P.ModuleName "Data.HeytingAlgebra") $ Left $ P.Ident "disj")

selectImportValueFixity :: Connection -> P.ModuleName -> P.OpName 'P.ValueOpName -> IO (Maybe ExternsFixity)
selectImportValueFixity conn modName opName = do
  SQL.query
    conn
    "SELECT associativity, precedence, op_name, alias_module_name, alias FROM value_operators WHERE module_name = ? AND op_name = ?"
    (modName, opName)
    <&> head

selectImportTypeFixities :: Connection -> P.Env -> P.ModuleName -> ImportDeclarationType -> IO (P.ModuleName, [ExternsTypeFixity])
selectImportTypeFixities conn env modName = fmap (fmap (modName,) )\case
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

selectTypeFixitiesFromExports :: Connection -> Map (P.OpName 'P.TypeOpName) P.ExportSource -> IO [ExternsTypeFixity]
selectTypeFixitiesFromExports conn = fmap catMaybes . mapConcurrently select . Map.toList
  where
    select (opName, exSrc) = selectImportTypeFixity conn (P.exportSourceDefinedIn exSrc) opName

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

selectEnvFromImports :: (MonadIO m) => Connection -> P.Module -> m E.Environment
selectEnvFromImports conn (P.Module _ _ modName decls exports) = liftIO do
  insertExports conn modName exports
  insertImports conn modName decls
  importFns :: [E.Environment -> E.Environment] <- forConcurrently decls \case
    P.ImportDeclaration _ mName idt _ -> do
      case idt of
        P.Implicit -> importModule mName
        P.Explicit refs -> importRefs mName refs
        P.Hiding refs -> importModuleHiding mName refs
    _ -> pure identity

  let env = foldl' (&) E.initEnvironment importFns

  envConstraintFns <- forConcurrently (getEnvConstraints env) \c -> do
    let (classMod, className) = toDbQualifer $ constraintClass c
    importClass classMod className

  pure $ foldl' (&) env envConstraintFns
  where
    importRefs mName refs = do
      edits :: [E.Environment -> E.Environment] <- forConcurrently refs (importRef mName)
      pure $ foldl' (>>>) identity edits

    importRef :: P.ModuleName -> P.DeclarationRef -> IO (E.Environment -> E.Environment)
    importRef mName = \case
      P.TypeClassRef _ className -> importClass mName className
      P.TypeRef _ tyName ctrs -> do
        let qual = P.Qualified (P.ByModuleName mName) tyName
        type' <- selectType conn qual
        ctrVals <- case ctrs of
          Nothing -> selectTypeDataConstructors conn qual
          Just ctrs' -> forConcurrently ctrs' \ctr -> do
            let qual' = P.Qualified (P.ByModuleName mName) ctr
            val <- selectDataConstructor conn qual'
            pure (qual', fromJustWithErr qual' val)
        pure $ \env' ->
          env'
            { E.types = E.types env' <> Map.fromList [(qual, fromJust type')],
              E.dataConstructors = E.dataConstructors env' <> Map.fromList ctrVals
            }
      P.ValueRef _ ident -> do
        let qual = P.Qualified (P.ByModuleName mName) ident
        val <- selectEnvValue conn qual
        pure $ \env' -> env' {E.names = E.names env' <> Map.fromList [(qual, fromJustWithErr qual val)]}
      P.TypeInstanceRef _ ident _ -> do
        let qual = P.Qualified (P.ByModuleName mName) ident
        val <- selectClassInstance conn qual
        pure $ \env' -> env' {E.typeClassDictionaries = P.addDictsToEnvMap [fromJust val] (E.typeClassDictionaries env')}
      P.ModuleRef _ m -> importModule m
      P.ReExportRef _ _ ref -> importRef mName ref
      P.ValueOpRef _ opName -> do
        (aliasModName, alias) <- fromJust <$> selectValueOperatorAlias conn mName opName
        if isUpper $ T.head alias
          then do
            let qual = P.Qualified (P.ByModuleName aliasModName) (P.ProperName alias)
            val <- selectDataConstructor conn qual
            pure $ \env' -> env' {E.dataConstructors = E.dataConstructors env' <> Map.fromList [(qual, fromJustWithErr qual val)]}
          else do
            let qual = P.Qualified (P.ByModuleName aliasModName) (P.Ident alias)
            val <- selectEnvValue conn qual
            pure $ \env' -> env' {E.names = E.names env' <> Map.fromList [(qual, fromJustWithErr qual val)]}
      P.TypeOpRef _ opName -> do
        (aliasModName, alias) <- fromJustWithErr opName <$> selectTypeOperatorAlias conn mName opName
        let qual = P.Qualified (P.ByModuleName aliasModName) alias
        val <- selectType conn qual
        pure $ \env' ->
          env'
            { E.types = E.types env' <> Map.fromList [(qual, fromJustWithErr qual val)]
            }

    importModule mName = importModuleHiding mName []

    importModuleHiding mName hideRefs = do
      allRefs <- selectModuleExports conn mName
      let refs = filter (not . flip Set.member hiddenRefSet) allRefs
      importRefs mName refs
      where
        hiddenRefSet = Set.fromList hideRefs

    importClass :: P.ModuleName -> P.ProperName 'P.ClassName -> IO (E.Environment -> E.Environment)
    importClass mName className = do
      let qual = P.Qualified (P.ByModuleName mName) className
          typeQual = P.Qualified (P.ByModuleName mName) $ coerceProperName className
      typeClass <- fromJust <$> selectTypeClass conn mName className
      type' <- selectType conn typeQual
      let dictName = P.Qualified (P.ByModuleName mName) . P.dictTypeName . coerceProperName $ className
      dictVal <- selectType conn dictName

      let ctrMb :: Maybe (P.Qualified (P.ProperName 'P.ConstructorName))
          ctrMb =
            P.Qualified (P.ByModuleName mName) <$> case dictVal of
              Just (_, P.DataType _ _ [(ctr', _)]) -> Just ctr'
              _ -> Nothing

      ctrData <- ctrMb & maybe (pure Nothing) (\ctr -> selectDataConstructor conn ctr)
      superClassImports <- forConcurrently (typeClassSuperclasses typeClass) \super -> case P.constraintClass super of
        P.Qualified (P.ByModuleName superModName) superClassName -> do
          importClass superModName superClassName
        _ -> pure identity
      instances <- selectClassInstancesByClassName conn qual

      pure $
        foldl' (>>>) identity superClassImports >>> \env' ->
          env'
            { E.typeClasses = E.typeClasses env' <> Map.fromList [(qual, typeClass)],
              E.types =
                E.types env'
                  <> Map.fromList
                    ( [ (typeQual, fromJust type')
                      ]
                        <> case dictVal of
                          Just val -> [(dictName, val)]
                          _ -> []
                    ),
              E.dataConstructors =
                E.dataConstructors env'
                  <> Map.fromList case (ctrMb, ctrData) of
                    (Just ctr', Just ctrData') -> [(ctr', ctrData')]
                    _ -> [],
              E.typeClassDictionaries = P.addDictsToEnvMap instances (E.typeClassDictionaries env')
            }

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

--   deserialiseIdents (ddt, ty, st, idents) = (ddt, ty, st, deserialise idents)

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

selectModuleTypeClasses :: Connection -> P.ModuleName -> IO [(P.Qualified (P.ProperName 'P.ClassName), P.TypeClassData)]
selectModuleTypeClasses conn moduleName' = do
  SQL.query
    conn
    "SELECT class_name, class FROM env_type_classes WHERE module_name = ?"
    (SQL.Only moduleName')
    <&> fmap (first (P.Qualified (P.ByModuleName moduleName')))

selectClassInstance ::
  Connection ->
  P.Qualified P.Ident ->
  IO (Maybe NamedDict)
selectClassInstance conn ident = do
  SQL.query
    conn
    "SELECT dict FROM env_type_class_instances WHERE module_name = ? AND instance_name = ?"
    (toDbQualifer ident)
    <&> (head >>> fmap (SQL.fromOnly >>> deserialise))

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

getEnvConstraints :: E.Environment -> [P.SourceConstraint]
getEnvConstraints env =
  E.names env & Map.elems >>= typeConstraints . view _1

typeConstraints :: P.Type a -> [P.Constraint a]
typeConstraints = P.everythingOnTypes (<>) \case
  P.ConstrainedType _ c _ -> [c]
  _ -> []

