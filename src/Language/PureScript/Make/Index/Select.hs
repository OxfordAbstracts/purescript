{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Make.Index.Select where

import Codec.Serialise (deserialise)
import Control.Arrow ((>>>))
import Control.Concurrent.Async.Lifted (mapConcurrently)
import Control.Monad.Writer (MonadWriter (tell), execWriter)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.AST.Traversals qualified as P
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment qualified as E
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs (ExternsFixity, ExternsTypeFixity)
import Language.PureScript.Names qualified as P
import Language.PureScript.Sugar.Names.Env qualified as P
import Language.PureScript.TypeChecker.Monad qualified as P
import Language.PureScript.TypeClassDictionaries (NamedDict)
import Language.PureScript.Types (Constraint (..))
import Language.PureScript.Types qualified as P
import Protolude hiding (moduleName)

selectFixitiesFromModule :: Connection -> P.Module -> IO ([(P.ModuleName, [ExternsFixity])], [(P.ModuleName, [ExternsTypeFixity])])
selectFixitiesFromModule conn (P.Module _ _ _ decls _) = do
  fixities <- selectValueFixities conn opNames
  typeFixities <- selectTypeFixities conn typeOpNames
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

selectValueFixities :: Connection -> [P.Qualified (P.OpName 'P.ValueOpName)] -> IO [(P.ModuleName, [ExternsFixity])]
selectValueFixities conn ops = collectModuleNames . catMaybes <$> mapConcurrently (selectValueFixity conn) ops

-- TODO: select all in module at one go for better performance
selectValueFixity :: Connection -> P.Qualified (P.OpName 'P.ValueOpName) -> IO (Maybe (P.ModuleName, ExternsFixity))
selectValueFixity conn (P.Qualified (P.ByModuleName m) op) =
  SQL.query
    conn
    "SELECT associativity, precedence, op_name, alias FROM env_value_fixities WHERE op_name = ? and module_name = ?"
    (op, m)
    <&> fmap (m,) . head
selectValueFixity _ _ = internalError "selectValueFixity called with BySourcePos"

selectTypeFixities :: Connection -> [P.Qualified (P.OpName 'P.TypeOpName)] -> IO [(P.ModuleName, [ExternsTypeFixity])]
selectTypeFixities conn ops = collectModuleNames . catMaybes <$> mapConcurrently (selectTypeFixity conn) ops

selectTypeFixity :: Connection -> P.Qualified (P.OpName 'P.TypeOpName) -> IO (Maybe (P.ModuleName, ExternsTypeFixity))
selectTypeFixity conn (P.Qualified (P.ByModuleName m) op) =
  SQL.query
    conn
    "SELECT  associativity, precedence, op_name, alias FROM env_type_fixities WHERE op_name = ? and module_name = ?"
    (op, m)
    <&> fmap (m,) . head
selectTypeFixity _ _ = internalError "selectTypeFixity called with BySourcePos"

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

selectEnvFromImports :: (MonadIO m) => Connection -> P.Module -> P.Env -> m E.Environment
selectEnvFromImports conn (P.Module _ _ moduleName' decls _) env = do
  case Map.lookup moduleName' env of
    Just (_, P.Imports {..}, _) -> liftIO do
      putErrLn (show moduleName' :: Text)
      when (moduleName' == P.ModuleName "Data.Exists") do
        putErrLn ("selectEnvFromImports" :: Text)
        print importedTypes
      names <- selectWithKeys importedValues selectEnvValue
      types <- selectWithKeys importedTypes selectType
      typeSynonyms <- selectWithKeys importedTypes selectTypeSynonym
      dataConstructors <- selectWithKeys importedDataConstructors selectDataConstructor
      typeClasses <- selectWithKeys importedTypeClasses selectTypeClass
      dicts <- selectDictsByClassName conn dictionaryClassnames
      pure $ E.Environment names (P.allPrimTypes <> types) dataConstructors typeSynonyms (P.typeClassDictionariesEnvMap dicts) (P.allPrimClasses <> typeClasses)
    Nothing -> internalError $ "selectEnvFromImports: module not found in env: " <> show moduleName'
  where
    selectWithKeys :: (Ord a, Show a) => Map.Map a x -> (Connection -> a -> IO (Maybe b)) -> IO (Map.Map a b)
    selectWithKeys a sel = a & Map.keys & mapConcurrently selWithKey <&> Map.fromList
      where
        selWithKey key = do
          val <- sel conn key
          case val of
            Nothing -> internalError $ "selectEnvFromImports: key not found: " <> show key
            Just val' -> pure (key, val')

    dictionaryClassnames :: [P.Qualified (P.ProperName 'P.ClassName)]
    dictionaryClassnames = execWriter . onDecls =<< decls
      where
        (onDecls, _, _) = P.everywhereOnValuesM getDeclClasses getExprClasses pure

        getDeclClasses e = do
          case e of
            P.TypeClassDeclaration _ c _ _ _ _ -> tell [P.Qualified (P.ByModuleName moduleName') c]
            _ -> pure ()
          pure e

        getExprClasses e = do
          case e of
            P.TypeClassDictionary c _ _ -> tell [constraintClass c]
            P.DeferredDictionary c _ -> tell [c]
            P.DerivedInstancePlaceholder c _ -> tell [c]
            _ -> pure ()
          pure e

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

selectDictsByClassName :: Connection -> [P.Qualified (P.ProperName 'P.ClassName)] -> IO [NamedDict]
selectDictsByClassName conn classNames =
  SQL.query
    conn
    "SELECT dict FROM env_type_class_instances WHERE class_name IN (SELECT value FROM json_each(?))"
    (SQL.Only $ A.encode classNames)
    <&> fmap (SQL.fromOnly >>> deserialise)

type DbQualifer a = (P.ModuleName, a)

toDbQualifer :: P.Qualified a -> DbQualifer a
toDbQualifer (P.Qualified (P.ByModuleName mn) a) = (mn, a)
toDbQualifer (P.Qualified (P.BySourcePos _) _) = internalError "toDbQualifer called with BySourcePos"
