{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Language.PureScript.Make.Index.Select where

import Codec.Serialise (deserialise)
import Control.Arrow ((>>>))
import Control.Concurrent.Async.Lifted (forConcurrently, forConcurrently_, mapConcurrently_)
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
import Language.PureScript.Environment (TypeClassData (typeClassSuperclasses))
import Language.PureScript.Environment qualified as E
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs (ExternsFixity (..), ExternsTypeFixity (..))
import Language.PureScript.Names (coerceProperName)
import Language.PureScript.Names qualified as P
import Language.PureScript.TypeChecker.Monad qualified as P
import Language.PureScript.TypeClassDictionaries (NamedDict)
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
selectFixitiesFromModuleImports conn (P.Module _ _ _ decls _refs) = do
  valueOps <- catMaybes <$> forConcurrently decls (onImports (selectImportValueFixities conn))
  typeOps <- catMaybes <$> forConcurrently decls (onImports (selectImportTypeFixities conn))
  pure (valueOps, typeOps)
  where
    onImports :: (P.ModuleName -> ImportDeclarationType -> IO (P.ModuleName, [a])) -> P.Declaration -> IO (Maybe (P.ModuleName, [a]))
    onImports f = \case
      P.ImportDeclaration _ mn' idt _ -> Just <$> f mn' idt
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

    -- P.TypeInstanceDeclaration _ _ _ _ _ deps _className _types _ -> do
    --   depFns <- forConcurrently deps \case
    --     dep -> do
    --       case P.constraintClass dep of
    --         -- P.Qualified (P.ByModuleName depModuleName) depClassName ->
    --         --   importClassAndTypes depModuleName depClassName
    --         _ -> pure identity

    -- pure (foldl' (>>>) identity depFns)

    -- dict <- selectClassInstanceByIdents conn className types
    -- pure $ \env' -> env' {E.typeClassDictionaries = E.typeClassDictionaries env' <> P.typeClassDictionariesEnvMap [fromJust dict]}
    _ -> pure identity

  dictFns <- forConcurrently deferredDicts \case
    (P.Qualified (P.ByModuleName mn) className, _types) -> importClassAndTypes mn className
    _ -> pure identity

  let env = foldl' (&) E.initEnvironment (importFns <> dictFns)
  pure env
  where
    deferredDicts = getDeclDicts =<< decls

    getDeclDicts :: P.Declaration -> [(P.Qualified (P.ProperName 'P.ClassName), [P.SourceType])]

    getDeclDicts d = execState (onDecl d) []

    (onDecl, _, _) = P.everywhereOnValuesM pure goExpr pure

    goExpr :: P.Expr -> State [(P.Qualified (P.ProperName 'P.ClassName), [P.SourceType])] P.Expr
    goExpr = \case
      e@(P.DeferredDictionary cn tys) -> do
        modify ((cn, tys) :)
        pure e
      e -> pure e

    importRefs mName refs = do
      edits :: [E.Environment -> E.Environment] <- forConcurrently refs (importRef mName)
      pure $ foldl' (>>>) identity edits

    importRef :: P.ModuleName -> P.DeclarationRef -> IO (E.Environment -> E.Environment)
    importRef mName = \case
      P.TypeClassRef _ className -> importClassAndTypes mName className
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
        pure $ \env' -> env' {E.typeClassDictionaries = P.addDictsToEnvMap [fromJust val] (E.typeClassDictionaries env')}

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

    importModule mName = importModuleHiding mName []

    importModuleHiding mName hideRefs = do
      allRefs <- selectModuleExports conn mName
      let refs = filter (not . flip Set.member hiddenRefSet) allRefs
      importRefs mName refs
      where
        hiddenRefSet = Set.fromList hideRefs

    importClassAndTypes :: P.ModuleName -> P.ProperName 'P.ClassName -> IO (E.Environment -> E.Environment)
    importClassAndTypes mName className = do
      let qual = P.Qualified (P.ByModuleName mName) className
          typeQual = P.Qualified (P.ByModuleName mName) $ coerceProperName className
      typeClass <- fromJust <$> selectTypeClass conn mName className
      type' <- selectType conn typeQual
      let dictName = P.Qualified (P.ByModuleName mName) . P.dictTypeName . coerceProperName $ className
      dictVal@(_, dictKind) <- fromJustWithErr dictName <$> selectType conn dictName

      let ctrMb :: Maybe (P.Qualified (P.ProperName 'P.ConstructorName))
          ctrMb =
            P.Qualified (P.ByModuleName mName) <$> case dictKind of
              P.DataType _ _ [(ctr', _)] -> Just ctr'
              _ -> Nothing

      ctrData <- ctrMb & maybe (pure Nothing) (\ctr -> selectDataConstructor conn ctr)
      superClassImports <- forConcurrently (typeClassSuperclasses typeClass) \super -> case P.constraintClass super of
        P.Qualified (P.ByModuleName superModName) superClassName -> do
          importClassAndTypes superModName superClassName
        _ -> pure identity
      instances <- selectClassInstancesByClassName conn qual

      pure $
        foldl' (>>>) identity superClassImports >>> \env' ->
          env'
            { E.typeClasses = E.typeClasses env' <> Map.fromList [(qual, typeClass)],
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
