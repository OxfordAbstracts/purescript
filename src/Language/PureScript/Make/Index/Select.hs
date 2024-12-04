{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Make.Index.Select where

import Codec.Serialise (deserialise)
import Control.Arrow ((>>>))
import Control.Concurrent.Async.Lifted (forConcurrently)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as Lazy
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Language.PureScript.AST.Declarations (ImportDeclarationType)
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.AST.Operators qualified as P
import Language.PureScript.AST.Traversals qualified as P
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment qualified as E
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs (ExternsFixity (..), ExternsTypeFixity (..))
import Language.PureScript.Names (coerceProperName)
import Language.PureScript.Names qualified as P
import Language.PureScript.TypeChecker.Monad qualified as P
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope (tcdValue))
import Language.PureScript.Types qualified as P
import Protolude hiding (moduleName)
import Protolude.Partial (fromJust)

selectFixitiesFromModuleImportsAndDecls :: Connection -> P.Module -> IO ([(P.ModuleName, [ExternsFixity])], [(P.ModuleName, [ExternsTypeFixity])])
selectFixitiesFromModuleImportsAndDecls conn module' = do
  (fixitiesFromImports, typeFixitiesFromImports) <- selectFixitiesFromModuleImports conn module'
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

selectFixitiesFromModuleImports :: Connection -> P.Module -> IO ([(P.ModuleName, [ExternsFixity])], [(P.ModuleName, [ExternsTypeFixity])])
selectFixitiesFromModuleImports conn (P.Module _ _ _ decls _) = do
  valueOps <- catMaybes <$> forConcurrently decls (onImports (selectImportValueFixities conn))
  typeOps <- catMaybes <$> forConcurrently decls (onImports (selectImportTypeFixities conn))
  pure (valueOps, typeOps)
  where
    onImports :: (P.ModuleName -> ImportDeclarationType -> IO (P.ModuleName, [a])) -> P.Declaration -> IO (Maybe (P.ModuleName, [a]))
    onImports f = \case
      P.ImportDeclaration _ mn idt _ -> Just <$> f mn idt
      _ -> pure Nothing

selectImportValueFixities :: Connection -> P.ModuleName -> ImportDeclarationType -> IO (P.ModuleName, [ExternsFixity])
selectImportValueFixities conn modName = \case
  P.Implicit -> selectValueFixitiesFromModule conn modName
  P.Explicit refs | refsValueOps refs /= [] -> selectExplicitValueFixitiesFromModule conn modName (refsValueOps refs)
  P.Hiding refs -> selectNonHiddenValueFixitiesFromModule conn modName (refsValueOps refs)
  _ -> pure (modName, [])

refsValueOps :: [P.DeclarationRef] -> [P.OpName 'P.ValueOpName]
refsValueOps = mapMaybe refValueOp

refValueOp :: P.DeclarationRef -> Maybe (P.OpName 'P.ValueOpName)
refValueOp = \case
  P.ValueOpRef _ ident -> Just ident
  _ -> Nothing

selectValueFixitiesFromModule :: Connection -> P.ModuleName -> IO (P.ModuleName, [ExternsFixity])
selectValueFixitiesFromModule conn modName = do
  (modName,)
    <$> SQL.query
      conn
      "SELECT associativity, precedence, op_name, alias_module_name, alias FROM value_operators WHERE module_name = ?"
      (SQL.Only modName)

selectExplicitValueFixitiesFromModule :: Connection -> P.ModuleName -> [P.OpName 'P.ValueOpName] -> IO (P.ModuleName, [ExternsFixity])
selectExplicitValueFixitiesFromModule _ modName [] = pure (modName, [])
selectExplicitValueFixitiesFromModule conn modName ops = do
  (modName,)
    <$> SQL.query
      conn
      "SELECT associativity, precedence, op_name, alias_module_name, alias FROM value_operators WHERE module_name = ? AND op_name IN (SELECT value FROM json_each(?))"
      (modName, decodeUtf8 $ Lazy.toStrict $ A.encode (fmap P.runOpName ops))

selectNonHiddenValueFixitiesFromModule :: Connection -> P.ModuleName -> [P.OpName 'P.ValueOpName] -> IO (P.ModuleName, [ExternsFixity])
selectNonHiddenValueFixitiesFromModule conn modName [] = selectValueFixitiesFromModule conn modName
selectNonHiddenValueFixitiesFromModule conn modName ops = do
  (modName,)
    <$> SQL.query
      conn
      "SELECT associativity, precedence, op_name, alias_module_name, alias FROM value_operators WHERE module_name = ? AND op_name NOT IN (SELECT value FROM json_each(?))"
      (modName, decodeUtf8 $ Lazy.toStrict $ A.encode (fmap P.runOpName ops))

byteToText :: Lazy.ByteString -> Text
byteToText = decodeUtf8 . Lazy.toStrict

selectImportTypeFixities :: Connection -> P.ModuleName -> ImportDeclarationType -> IO (P.ModuleName, [ExternsTypeFixity])
selectImportTypeFixities conn modName = \case
  P.Implicit -> selectTypeFixitiesFromModule conn modName
  P.Explicit refs | refsTypeOps refs /= [] -> selectExplicitTypeFixitiesFromModule conn modName (refsTypeOps refs)
  P.Hiding refs -> selectNonHiddenTypeFixitiesFromModule conn modName (refsTypeOps refs)
  _ -> pure (modName, [])

refsTypeOps :: [P.DeclarationRef] -> [P.OpName 'P.TypeOpName]
refsTypeOps = mapMaybe refTypeOp

refTypeOp :: P.DeclarationRef -> Maybe (P.OpName 'P.TypeOpName)
refTypeOp = \case
  P.TypeOpRef _ ident -> Just ident
  _ -> Nothing

selectTypeFixitiesFromModule :: Connection -> P.ModuleName -> IO (P.ModuleName, [ExternsTypeFixity])
selectTypeFixitiesFromModule conn modName = do
  (modName,)
    <$> SQL.query
      conn
      "SELECT associativity, precedence, op_name, alias_module_name, alias FROM type_operators WHERE module_name = ?"
      (SQL.Only modName)

selectExplicitTypeFixitiesFromModule :: Connection -> P.ModuleName -> [P.OpName 'P.TypeOpName] -> IO (P.ModuleName, [ExternsTypeFixity])
selectExplicitTypeFixitiesFromModule _ modName [] = pure (modName, [])
selectExplicitTypeFixitiesFromModule conn modName ops = do
  (modName,)
    <$> SQL.query
      conn
      "SELECT associativity, precedence, op_name, alias_module_name, alias FROM type_operators WHERE module_name = ? AND op_name IN (SELECT value FROM json_each(?))"
      (modName, decodeUtf8 $ Lazy.toStrict $ A.encode (fmap P.runOpName ops))

selectNonHiddenTypeFixitiesFromModule :: Connection -> P.ModuleName -> [P.OpName 'P.TypeOpName] -> IO (P.ModuleName, [ExternsTypeFixity])
selectNonHiddenTypeFixitiesFromModule conn modName [] = selectTypeFixitiesFromModule conn modName
selectNonHiddenTypeFixitiesFromModule conn modName ops = do
  (modName,)
    <$> SQL.query
      conn
      "SELECT associativity, precedence, op_name, alias_module_name, alias FROM type_operators WHERE module_name = ? AND op_name NOT IN (SELECT value FROM json_each(?))"
      (modName, decodeUtf8 $ Lazy.toStrict $ A.encode (fmap P.runOpName ops))


type ClassDict =
  Map.Map
    P.QualifiedBy
    ( Map.Map
        (P.Qualified (P.ProperName 'P.ClassName))
        (Map.Map (P.Qualified P.Ident) (NEL.NonEmpty NamedDict))
    )

unionClassDicts :: [ClassDict] -> ClassDict
unionClassDicts = foldl' unionClassDict Map.empty

unionClassDict :: ClassDict -> ClassDict -> ClassDict
unionClassDict = Map.unionWith (Map.unionWith (Map.unionWith (<>)))

collectModuleNames :: (Ord a) => [(P.ModuleName, a)] -> [(P.ModuleName, [a])]
collectModuleNames = Map.toList . Map.fromListWith (<>) . fmap (fmap pure) . ordNub

classDictNamedDicts :: ClassDict -> [NamedDict]
classDictNamedDicts = concatMap NEL.toList . concatMap Map.elems . concatMap Map.elems . Map.elems

selectEnvFromImports :: (MonadIO m) => Connection -> P.Module -> m E.Environment
selectEnvFromImports conn (P.Module _ _ _ decls _) = liftIO do
  importFns :: [E.Environment -> E.Environment] <- forConcurrently decls \case
    P.ImportDeclaration _ mName idt _ -> do
      case idt of
        P.Implicit -> importModule mName
        P.Explicit refs -> do
          edits :: [E.Environment -> E.Environment] <- forConcurrently refs (importRef mName)
          pure $ foldl' (>>>) identity edits
        P.Hiding refs -> importModuleHiding refs mName
    _ -> pure identity

  putErrLn $ ("dicts: "  :: Text) <> show dicts
  constraintFns <- forConcurrently typeConstraints \c ->
    case P.constraintClass c of
      P.Qualified (P.ByModuleName mName) className ->
        importClass mName className
      _ -> pure identity

  pure $ foldl' (&) E.initEnvironment (constraintFns <> importFns)
  where
    dicts = classDictNamedDicts $ unionClassDicts $ getDeclDicts <$> decls

    getDeclDicts :: P.Declaration -> ClassDict
    getDeclDicts d = execState (onDecl d) Map.empty

    (onDecl, _, _) = P.everywhereOnValuesM pure goExpr pure 

    goExpr :: P.Expr -> State ClassDict P.Expr
    goExpr = \case
      e@(P.TypeClassDictionary _ dict _) -> do  
        modify (unionClassDict dict)
        pure e
      e -> pure e

    typeConstraints :: [P.SourceConstraint]
    typeConstraints = getDeclTypeConstraints =<< decls

    (getDeclTypeConstraints, _, _, _, _) = P.accumTypes \case
      P.ConstrainedType _ c _ -> [c]
      _ -> []

    importModule = importModuleHiding []

    -- todo: select imports from db and use same code as explicit imports
    importModuleHiding hideRefs mName = do
      let hiddenIdents =
            Set.fromList $
              hideRefs >>= \case
                P.ValueRef _ ident -> [ident]
                _ -> []

          hiddenTypes =
            Set.fromList $
              hideRefs >>= \case
                P.TypeRef _ tyName _ -> [tyName]
                _ -> []

          hiddenCtrs =
            Set.fromList $
              hideRefs >>= \case
                P.TypeRef _ _ ctrs -> fold ctrs
                _ -> []
          hiddenTypeClasses =
            Set.fromList $
              hideRefs >>= \case
                P.TypeClassRef _ className -> [className]
                _ -> []

          hiddenInstances =
            Set.fromList $
              hideRefs >>= \case
                P.TypeInstanceRef _ ident _ -> [ident]
                _ -> []

      names <-
        filter (\(ident, _) -> not $ Set.member (P.disqualify ident) hiddenIdents)
          <$> selectModuleEnvValues conn mName
      types <-
        filter (\(ty, _) -> not $ Set.member (P.disqualify ty) hiddenTypes)
          <$> selectModuleEnvTypes conn mName
      dataConstructors <-
        filter (\(ctr, _) -> not $ Set.member (P.disqualify ctr) hiddenCtrs)
          <$> selectModuleDataConstructors conn mName
      typeSynonyms <-
        filter (\(ty, _) -> not $ Set.member (P.disqualify ty) hiddenTypes)
          <$> selectModuleTypeSynonyms conn mName
      typeClasses <-
        filter (\(tc, _) -> not $ Set.member (P.disqualify tc) hiddenTypeClasses)
          <$> selectModuleTypeClasses conn mName
      instances <-
        filter (\inst -> not $ Set.member (P.disqualify $ tcdValue inst) hiddenInstances)
          <$> selectModuleClassInstances conn mName
      pure $ \env' ->
        env'
          { E.names = E.names env' <> Map.fromList names,
            E.types = E.types env' <> Map.fromList types,
            E.dataConstructors = E.dataConstructors env' <> Map.fromList dataConstructors,
            E.typeSynonyms = E.typeSynonyms env' <> Map.fromList typeSynonyms,
            E.typeClasses = E.typeClasses env' <> Map.fromList typeClasses,
            E.typeClassDictionaries = E.typeClassDictionaries env' <> P.typeClassDictionariesEnvMap instances
          }

    importClass mName className = do
      let qual = P.Qualified (P.ByModuleName mName) className
          typeQual = P.Qualified (P.ByModuleName mName) $ coerceProperName className
      typeClass <- selectTypeClass conn qual
      type' <- selectType conn typeQual
      let dictName = P.Qualified (P.ByModuleName mName) . P.dictTypeName . coerceProperName $ className
      dictVal@(_, dictKind) <- fromJust <$> selectType conn dictName

      let ctrMb :: Maybe (P.Qualified (P.ProperName 'P.ConstructorName))
          ctrMb =
            P.Qualified (P.ByModuleName mName) <$> case dictKind of
              P.DataType _ _ [(ctr', _)] -> Just ctr'
              _ -> Nothing

      ctrData <- ctrMb & maybe (pure Nothing) (\ctr -> selectDataConstructor conn ctr)
      pure $ \env' ->
        env'
          { E.typeClasses = E.typeClasses env' <> Map.fromList [(qual, fromJust typeClass)],
            E.types =
              E.types env'
                <> Map.fromList
                  [ (typeQual, fromJust type'),
                    (dictName, dictVal)
                  ],
            E.dataConstructors =
              E.dataConstructors env'
                <> Map.fromList case (ctrMb, ctrData) of
                  (Just ctr', Just ctrData') -> [(ctr', ctrData')]
                  _ -> []
          }

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
            pure (qual', fromJust val)
        pure $ \env' ->
          env'
            { E.types = E.types env' <> Map.fromList [(qual, fromJust type')],
              E.dataConstructors = E.dataConstructors env' <> Map.fromList ctrVals
            }
      P.ValueRef _ ident -> do
        let qual = P.Qualified (P.ByModuleName mName) ident
        val <- selectEnvValue conn qual
        pure $ \env' -> env' {E.names = E.names env' <> Map.fromList [(qual, fromJust val)]}
      P.TypeInstanceRef _ ident _ -> do
        let qual = P.Qualified (P.ByModuleName mName) ident
        val <- selectClassInstance conn qual
        pure $ \env' -> env' {E.typeClassDictionaries = E.typeClassDictionaries env' <> P.typeClassDictionariesEnvMap [fromJust val]}
      P.ModuleRef _ m -> importModule m
      P.ReExportRef _ _ ref -> importRef mName ref
      P.ValueOpRef _ opName -> do
        (aliasModName, alias) <- fromJust <$> selectValueOperatorAlias conn mName opName
        if isUpper $ T.head alias
          then do
            let qual = P.Qualified (P.ByModuleName aliasModName) (P.ProperName alias)
            val <- selectDataConstructor conn qual
            pure $ \env' -> env' {E.dataConstructors = E.dataConstructors env' <> Map.fromList [(qual, fromJust val)]}
          else do
            let qual = P.Qualified (P.ByModuleName aliasModName) (P.Ident alias)
            val <- selectEnvValue conn qual
            pure $ \env' -> env' {E.names = E.names env' <> Map.fromList [(qual, fromJust val)]}
      P.TypeOpRef _ opName -> do
        (aliasModName, alias) <- fromJust <$> selectTypeOperatorAlias conn mName opName
        let qual = P.Qualified (P.ByModuleName aliasModName) alias
        val <- selectType conn qual
        pure $ \env' ->
          env'
            { E.types = E.types env' <> Map.fromList [(qual, fromJust val)]
            }

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

selectTypeClass :: Connection -> P.Qualified (P.ProperName 'P.ClassName) -> IO (Maybe P.TypeClassData)
selectTypeClass conn ident = case Map.lookup ident P.allPrimClasses of
  Just a -> pure $ Just a
  Nothing ->
    SQL.query
      conn
      "SELECT class FROM env_type_classes WHERE module_name = ? AND class_name = ?"
      (modName, className)
      <&> (fmap SQL.fromOnly . head)
  where
    (modName, className) = toDbQualifer ident

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
    "SELECT dict FROM env_type_class_instances WHERE module_name = ? AND class_name = ? AND idents = ?"
    (modName, className, A.encode idents)
    <&> (head >>> fmap (SQL.fromOnly >>> deserialise))
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

type DbQualifer a = (P.ModuleName, a)

toDbQualifer :: P.Qualified a -> DbQualifer a
toDbQualifer (P.Qualified (P.ByModuleName mn) a) = (mn, a)
toDbQualifer (P.Qualified (P.BySourcePos _) _) = internalError "toDbQualifer called with BySourcePos"

fromJustWithErr :: (Show e) => e -> Maybe a -> a
fromJustWithErr err = \case
  Just a -> a
  Nothing -> internalError $ "fromJustWithErr: " <> show err