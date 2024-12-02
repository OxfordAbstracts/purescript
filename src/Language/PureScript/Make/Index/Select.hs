{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Make.Index.Select where

import Codec.Serialise (deserialise)
import Control.Arrow ((>>>))
import Control.Concurrent.Async.Lifted (forConcurrently, mapConcurrently)
import Control.Monad.Writer (MonadWriter (tell), execWriter)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Data.Set qualified as Set
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.AST.Traversals qualified as P
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment qualified as E
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs (ExternsFixity, ExternsTypeFixity)
import Language.PureScript.Names qualified as P
import Language.PureScript.TypeChecker.Monad qualified as P
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope (tcdValue))
import Language.PureScript.Types qualified as P
import Protolude hiding (moduleName)
import Protolude.Partial (fromJust)

selectFixitiesFromModule :: Connection -> P.Module -> IO ([(P.ModuleName, [ExternsFixity])], [(P.ModuleName, [ExternsTypeFixity])])
selectFixitiesFromModule conn (P.Module _ _ modName decls _) = do
  fixities <- selectValueFixities conn modName opNames
  typeFixities <- selectTypeFixities conn modName typeOpNames
  pure (fixities, typeFixities)
  where
    opNames :: [P.Qualified (P.OpName 'P.ValueOpName)]
    opNames = execWriter . getDeclOps =<< decls

    (getDeclOps, _, _) = P.everywhereOnValuesM pure getExprOp pure

    getExprOp e = do
      case e of
        P.Op _ op -> tell [op] >> pure e
        _ -> pure e

    typeOpNames :: [P.Qualified (P.OpName 'P.TypeOpName)]
    typeOpNames = getDeclTypeOps =<< decls

    (getDeclTypeOps, _, _, _, _) = P.accumTypes \case
      P.TypeOp _ op -> [op]
      _ -> []

selectValueFixities :: Connection -> P.ModuleName -> [P.Qualified (P.OpName 'P.ValueOpName)] -> IO [(P.ModuleName, [ExternsFixity])]
selectValueFixities conn modName ops = collectModuleNames . catMaybes <$> mapConcurrently (selectValueFixity conn modName) ops

-- TODO: select all in module at one go for better performance
selectValueFixity :: Connection -> P.ModuleName -> P.Qualified (P.OpName 'P.ValueOpName) -> IO (Maybe (P.ModuleName, ExternsFixity))
selectValueFixity conn modName op =
  SQL.query
    conn
    "SELECT associativity, precedence, op_name, alias FROM value_operators WHERE op_name = ? and module_name = ?"
    (P.disqualify op, m)
    <&> fmap (m,) . head
    where 
      m = fromMaybe modName $ P.getQual op



selectTypeFixities :: Connection -> P.ModuleName -> [P.Qualified (P.OpName 'P.TypeOpName)] -> IO [(P.ModuleName, [ExternsTypeFixity])]
selectTypeFixities conn modName ops = collectModuleNames . catMaybes <$> mapConcurrently (selectTypeFixity conn modName) ops

selectTypeFixity :: Connection -> P.ModuleName -> P.Qualified (P.OpName 'P.TypeOpName) -> IO (Maybe (P.ModuleName, ExternsTypeFixity))
selectTypeFixity conn modName op =
  SQL.query
    conn
    "SELECT  associativity, precedence, op_name, alias FROM type_operators WHERE op_name = ? and module_name = ?"
    (P.disqualify op, m)
    <&> fmap (m,) . head
    where 
      m = fromMaybe modName $ P.getQual op


collectModuleNames :: [(P.ModuleName, a)] -> [(P.ModuleName, [a])]
collectModuleNames = Map.toList . Map.fromListWith (<>) . fmap (fmap pure)

selectEnv :: (MonadIO m) => Connection -> [P.ModuleName] -> m E.Environment
selectEnv conn deps = do
  values <- liftIO $ join <$> mapConcurrently (selectModuleEnvValues conn) deps
  types <- liftIO $ join <$> mapConcurrently (selectModuleEnvTypes conn) deps
  dataConstructors <- liftIO $ join <$> mapConcurrently (selectModuleDataConstructors conn) deps
  typeSynonyms <- liftIO $ join <$> mapConcurrently (selectModuleTypeSynonyms conn) deps
  typeClasses <- liftIO $ join <$> mapConcurrently (selectModuleTypeClasses conn) deps
  pure
    E.initEnvironment
      { E.names = Map.fromList values,
        E.types = Map.fromList types,
        E.dataConstructors = Map.fromList dataConstructors,
        E.typeSynonyms = Map.fromList typeSynonyms,
        E.typeClasses = Map.fromList typeClasses
      }

selectEnvFromImports :: (MonadIO m) => Connection -> P.Module -> m E.Environment
selectEnvFromImports conn (P.Module _ _ _ decls _) = liftIO do
  envFns :: [E.Environment -> E.Environment] <- forConcurrently decls \case
    P.ImportDeclaration _ mName idt _ -> do
      case idt of
        P.Implicit -> importModule mName
        P.Explicit refs -> do
          edits :: [E.Environment -> E.Environment] <- forConcurrently refs (importRef mName)
          pure $ foldl' (>>>) identity edits
        P.Hiding refs -> importModuleHiding refs mName
    _ -> pure identity
  pure $ foldl' (&) E.initEnvironment envFns
  where
    importModule = importModuleHiding []

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

    importRef :: P.ModuleName -> P.DeclarationRef -> IO (E.Environment -> E.Environment)
    importRef mName = \case
      P.TypeClassRef _ className -> do
        let qual = P.Qualified (P.ByModuleName mName) className
        typeClass <- selectTypeClass conn qual
        pure $ \env' -> env' {E.typeClasses = E.typeClasses env' <> Map.fromList [(qual, fromJust typeClass)]}
      P.TypeRef _ tyName ctrs -> do
        let qual = P.Qualified (P.ByModuleName mName) tyName
        type' <- selectType conn qual
        ctrVals <-  case ctrs of 
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
      _ -> pure identity

selectEnvValue :: Connection -> P.Qualified P.Ident -> IO (Maybe (P.SourceType, P.NameKind, P.NameVisibility))
selectEnvValue conn ident =
  SQL.query
    conn
    "SELECT source_type, name_kind, name_visibility FROM env_values WHERE module_name = ? AND ident = ?"
    (toDbQualifer ident)
    <&> head

selectModuleEnvValues :: Connection -> P.ModuleName -> IO [(P.Qualified P.Ident, (P.SourceType, P.NameKind, P.NameVisibility))]
selectModuleEnvValues conn moduleName' =
  SQL.query
    conn
    "SELECT ident, source_type, name_kind, name_visibility FROM env_values WHERE module_name = ?"
    (SQL.Only moduleName')
    <&> fmap (\(ident, st, nk, nv) -> (P.Qualified (P.ByModuleName moduleName') ident, (st, nk, nv)))

selectType :: Connection -> P.Qualified (P.ProperName 'P.TypeName) -> IO (Maybe (P.SourceType, P.TypeKind))
selectType conn ident =
  SQL.query
    conn
    "SELECT source_type, type_kind FROM env_types WHERE module_name = ? AND type_name = ?"
    (toDbQualifer ident)
    <&> head

selectModuleEnvTypes :: Connection -> P.ModuleName -> IO [(P.Qualified (P.ProperName 'P.TypeName), (P.SourceType, P.TypeKind))]
selectModuleEnvTypes conn moduleName' =
  SQL.query
    conn
    "SELECT type_name, source_type, type_kind FROM env_types WHERE module_name = ?"
    (SQL.Only moduleName')
    <&> fmap (\(ty, st, tk) -> (P.Qualified (P.ByModuleName moduleName') ty, (st, tk)))

selectDataConstructor :: Connection -> P.Qualified (P.ProperName 'P.ConstructorName) -> IO (Maybe (P.DataDeclType, P.ProperName 'P.TypeName, P.SourceType, [P.Ident]))
selectDataConstructor conn ident =
  SQL.query
    conn
    "SELECT data_decl_type, type_name, source_type, idents FROM env_data_constructors WHERE module_name = ? AND constructor_name = ?"
    (toDbQualifer ident)
    <&> (head >>> fmap deserialiseIdents)
  where
    deserialiseIdents (ddt, ty, st, idents) = (ddt, ty, st, deserialise idents)

selectTypeDataConstructors :: Connection -> P.Qualified (P.ProperName 'P.TypeName) -> IO [(P.Qualified (P.ProperName 'P.ConstructorName), (P.DataDeclType, P.ProperName 'P.TypeName, P.SourceType, [P.Ident]))]
selectTypeDataConstructors conn ident =
  SQL.query
    conn
    "SELECT constructor_name, data_decl_type, type_name, source_type, idents FROM env_data_constructors WHERE module_name = ? AND type_name = ?"
    (toDbQualifer ident)
    <&> fmap (\(ctr, ddt, ty, st, idents) -> (P.Qualified (P.ByModuleName moduleName') ctr, (ddt, ty, st, deserialise idents)))
  where
    moduleName' = fromJust $ P.getQual ident
  --   deserialiseIdents (ddt, ty, st, idents) = (ddt, ty, st, deserialise idents)
    
selectModuleDataConstructors :: Connection -> P.ModuleName -> IO [(P.Qualified (P.ProperName 'P.ConstructorName), (P.DataDeclType, P.ProperName 'P.TypeName, P.SourceType, [P.Ident]))]
selectModuleDataConstructors conn moduleName' =
  SQL.query
    conn
    "SELECT constructor_name, data_decl_type, type_name, source_type, idents FROM env_data_constructors WHERE module_name = ?"
    (SQL.Only moduleName')
    <&> fmap (\(ctr, ddt, ty, st, idents) -> (P.Qualified (P.ByModuleName moduleName') ctr, (ddt, ty, st, deserialise idents)))

selectTypeSynonym :: Connection -> P.Qualified (P.ProperName 'P.TypeName) -> IO (Maybe ([(Text, Maybe P.SourceType)], P.SourceType))
selectTypeSynonym conn ident =
  SQL.query
    conn
    "SELECT idents, source_type FROM env_type_synonyms WHERE module_name = ? AND type_name = ?"
    (toDbQualifer ident)
    <&> (head >>> fmap deserialiseIdents)
  where
    deserialiseIdents (idents, st) = (deserialise idents, st)

selectModuleTypeSynonyms :: Connection -> P.ModuleName -> IO [(P.Qualified (P.ProperName 'P.TypeName), ([(Text, Maybe P.SourceType)], P.SourceType))]
selectModuleTypeSynonyms conn moduleName' =
  SQL.query
    conn
    "SELECT type_name, idents, source_type FROM env_type_synonyms WHERE module_name = ?"
    (SQL.Only moduleName')
    <&> fmap (\(ty, idents, st) -> (P.Qualified (P.ByModuleName moduleName') ty, (deserialise idents, st)))

selectTypeClass :: Connection -> P.Qualified (P.ProperName 'P.ClassName) -> IO (Maybe P.TypeClassData)
selectTypeClass conn ident =
  SQL.query
    conn
    "SELECT class FROM env_type_classes WHERE module_name = ? AND class_name = ?"
    (toDbQualifer ident)
    <&> (fmap SQL.fromOnly . head)

selectModuleTypeClasses :: Connection -> P.ModuleName -> IO [(P.Qualified (P.ProperName 'P.ClassName), P.TypeClassData)]
selectModuleTypeClasses conn moduleName' =
  SQL.query
    conn
    "SELECT class_name, class FROM env_type_classes WHERE module_name = ?"
    (SQL.Only moduleName')
    <&> fmap (first (P.Qualified (P.ByModuleName moduleName')))

selectClassInstance ::
  Connection ->
  P.Qualified P.Ident ->
  IO (Maybe NamedDict)
selectClassInstance conn ident =
  SQL.query
    conn
    "SELECT dict FROM env_type_class_instances WHERE module_name = ? AND ident = ?"
    (toDbQualifer ident)
    <&> (head >>> fmap (SQL.fromOnly >>> deserialise))

selectModuleClassInstances :: Connection -> P.ModuleName -> IO [NamedDict]
selectModuleClassInstances conn moduleName' =
  SQL.query
    conn
    "SELECT dict FROM env_type_class_instances WHERE module_name = ?"
    (SQL.Only moduleName')
    <&> fmap (SQL.fromOnly >>> deserialise)

selectClassInstancesByClassName :: Connection -> [P.Qualified (P.ProperName 'P.ClassName)] -> IO [NamedDict]
selectClassInstancesByClassName conn classNames =
  SQL.query
    conn
    "SELECT dict FROM env_type_class_instances WHERE class_name IN (SELECT value FROM json_each(?))"
    (SQL.Only $ A.encode classNames)
    <&> fmap (SQL.fromOnly >>> deserialise)

type DbQualifer a = (P.ModuleName, a)

toDbQualifer :: P.Qualified a -> DbQualifer a
toDbQualifer (P.Qualified (P.ByModuleName mn) a) = (mn, a)
toDbQualifer (P.Qualified (P.BySourcePos _) _) = internalError "toDbQualifer called with BySourcePos"
