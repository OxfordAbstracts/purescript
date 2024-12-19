{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

{-# HLINT ignore "Redundant bracket" #-}

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
  typeOps <- onImports selectImportTypeFixities
  pure (valueOps, typeOps)
  where
    onImports ::
      (Connection -> P.Env -> P.ModuleName -> ImportDeclarationType -> IO [(P.ModuleName, a)]) ->
      IO [(P.ModuleName, [a])]
    onImports fn = groupByModule . join . catMaybes <$> forConcurrently decls (whenImportDecl (fn conn env))

    whenImportDecl :: (P.ModuleName -> ImportDeclarationType -> IO a) -> P.Declaration -> IO (Maybe a)
    whenImportDecl f = \case
      P.ImportDeclaration _ mn' idt _ -> Just <$> f mn' idt
      _ -> pure Nothing

    groupByModule :: [(P.ModuleName, a)] -> [(P.ModuleName, [a])]
    groupByModule = Map.toList . Map.fromListWith (<>) . fmap (fmap pure)

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

-- selectEnvFromDefinitions :: forall m. (MonadIO m) => Connection -> P.Env -> P.Module -> m E.Environment
-- selectEnvFromDefinitions conn _exportEnv (P.Module _ _ modName decls _) = liftIO do
--   -- when (modName == P.ModuleName "Data.BooleanAlgebra") do
--   --   putErrText "de"
--   --   putErrText "\n"
--   --   putErrText $ T.intercalate "\n\n" (show <$> decls)
--   updates <- catMaybes <$> forConcurrently usedNames (import' E.initEnvironment)
--   let env = pipe (fmap snd updates) E.initEnvironment
--   addEnvTypes env
--   where
--     addEnvTypes :: E.Environment -> IO E.Environment
--     addEnvTypes env = do
--       let toImport = getTypesToImportFromEnv env
--       updates <- catMaybes <$> forConcurrently (Set.toList $ getTypesToImportFromEnv env) (import' env)
--       putErrText $ show modName
--       when (modName == P.ModuleName "Data.Show") do
--         putErrText "\n"
--         putErrText $ T.pack $ intercalate "\n" (show <$> Set.toList toImport)
--         putErrText "\n"
--         -- putErrText $ T.pack $ intercalate "\n" (P.debugTypeClassDictionaries env)
--         putErrText "\n\n"
--         putErrText ("updates: " <> show (length updates))
--         putErrText (T.intercalate "\n" $ fmap (show . fst) updates)

--       let newEnv = pipe (fmap snd updates) env
--       case updates of
--         [] -> pure env
--         _ 
--           | newEnv /= env -> 
--             addEnvTypes $ pipe (fmap snd updates) env
--           | otherwise ->  pure env

--     usedNames = Set.toList $ Set.unions $ getUsedNames <$> decls

--     import' :: E.Environment -> P.Qualified ToImport -> IO (Maybe (P.Qualified ToImport, E.Environment -> E.Environment))
--     import' env ti@(P.Qualified (P.ByModuleName mName) name) | mName /= modName = fmap (ti,) <$> do
--       case name of
--         TiIdent ident -> do
--           let qual = P.Qualified (P.ByModuleName mName) ident
--           if Map.member qual (E.names env)
--             then pure Nothing
--             else do
--               val <- selectEnvValue conn qual
--               pure $ Just $ \env' -> env' {E.names = E.names env' <> Map.fromList [(qual, fromJustWithErr qual val)]}
--         TiType tyName -> do
--           let qual = P.Qualified (P.ByModuleName mName) tyName
--           if Map.member qual (E.types env) || Map.member qual (E.typeSynonyms env)
--             then pure Nothing
--             else do
--               type' <- selectType conn qual
--               pure $ Just $ \env' -> env' {E.types = E.types env' <> Map.fromList [(qual, fromJustWithErr qual type')]}
--         TiDctor ctrName -> do
--           let qual = P.Qualified (P.ByModuleName mName) ctrName
--           if Map.member qual (E.dataConstructors env)
--             then pure Nothing
--             else do
--               val <- selectDataConstructor conn qual
--               pure $ Just $ \env' -> env' {E.dataConstructors = E.dataConstructors env' <> Map.fromList [(qual, fromJust val)]}
--         TiClass className -> do
--           let qual = P.Qualified (P.ByModuleName mName) className
--           if Map.member qual (E.typeClasses env)
--             then pure Nothing
--             else do
--               typeClass <- selectTypeClass conn mName className
--               pure $ Just $ \env' -> env' {E.typeClasses = E.typeClasses env' <> Map.fromList [(qual, fromJust typeClass)]}
--         TiDeferredDictionary className -> do
--           let qual = P.Qualified (P.ByModuleName mName) className
--           instances <- selectClassInstancesByClassName conn qual

--           let dictInEnv _d =
--                 Map.lookup (P.ByModuleName mName) (E.typeClassDictionaries env)
--                   & maybe False (Map.member qual)
--                   -- & maybe False (Map.member (P.tcdValue d))

--           when (modName == P.ModuleName "Data.Show") do
--             putErrText $ T.pack $ intercalate "\n" (P.debugTypeClassDictionaries env)
--             putErrText $ "instances: "
--             for_ instances \i -> do 
--               putErrText $ show i
--               putErrText $ show $ dictInEnv i


--           if all dictInEnv instances
--               then pure Nothing
--               else pure $ Just $ \env' -> env' {E.typeClassDictionaries = P.addDictsToEnvMap instances (E.typeClassDictionaries env')}
--     import' _ _ = pure Nothing

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

-- (handleDecl, _, _) = P.everywhereOnValuesM onDecl onExpr pure

-- -- onDecl :: P.Declaration -> Writer (Set (P.Qualified P.ProperName)) P.Declaration
-- onDecl d = do
--   let (declTypeNames, _, _, _, _) = P.accumTypes (P.everythingOnTypes (<>) _)
--   tell $ Set.map (fmap P.coerceProperName) $ declTypeNames d
--   pure d
-- onExpr = pure

-- selectEnvFromImports :: (MonadIO m) => Connection -> P.Env -> P.UsedImports -> P.Module -> m E.Environment
-- selectEnvFromImports conn exportEnv _usedImports (P.Module _ _ modName decls exportedRefs) = liftIO do
--   insertExports conn modName exportedRefs
--   insertImports conn modName decls
--   importFn <-
--     ( onImportMap P.importedTypes \typeImport ->
--         do
--           let tyName = P.disqualify $ P.importName typeImport
--           synMb <- selectTypeSynonym' conn (P.importSourceModule typeImport) tyName
--           case synMb of
--             Just syn -> do
--               pure $ \env' ->
--                 env'
--                   { E.typeSynonyms =
--                       E.typeSynonyms env'
--                         <> Map.fromList
--                           [ (P.importName typeImport, syn),
--                             (P.Qualified (P.ByModuleName $ P.importSourceModule typeImport) tyName, syn)
--                           ]
--                   }
--             Nothing -> do
--               type' <- selectType' conn (P.importSourceModule typeImport) tyName
--               pure $ \env' ->
--                 env'
--                   { E.types =
--                       E.types env'
--                         <> Map.fromList
--                           [ (P.importName typeImport, fromJust type'),
--                             (P.Qualified (P.ByModuleName $ P.importSourceModule typeImport) tyName, fromJust type')
--                           ]
--                   }
--       )
--       `updateConcurrently` ( onImportMap P.importedDataConstructors \ctrImport ->
--                                do
--                                  let ctrName = P.disqualify $ P.importName ctrImport
--                                      qualified = P.Qualified (P.ByModuleName $ P.importSourceModule ctrImport) ctrName
--                                  ctr <- selectDataConstructor conn (P.Qualified (P.ByModuleName $ P.importSourceModule ctrImport) ctrName)
--                                  pure $ \env' ->
--                                    env'
--                                      { E.dataConstructors =
--                                          E.dataConstructors env'
--                                            <> Map.fromList
--                                              [ (P.importName ctrImport, fromJust ctr),
--                                                (qualified, fromJust ctr)
--                                              ]
--                                      }
--                            )
--       `updateConcurrently` ( onImportMap P.importedTypeClasses \classImport ->
--                                importClass (P.importSourceModule classImport) (P.importName classImport) (P.disqualify $ P.importName classImport)
--                            )
--       `updateConcurrently` ( onImportMap P.importedValues \valImport -> do
--                                let ident = P.disqualify $ P.importName valImport
--                                val <- selectEnvValue conn (P.Qualified (P.ByModuleName $ P.importSourceModule valImport) ident)
--                                pure $ \env' ->
--                                  env'
--                                    { E.names =
--                                        E.names env'
--                                          <> Map.fromList
--                                            [ ( P.importName valImport,
--                                                fromJustWithErr (modName, P.importSourceModule valImport, ident) val
--                                              ),
--                                              ( P.Qualified (P.ByModuleName $ P.importSourceModule valImport) ident,
--                                                fromJustWithErr (modName, P.importSourceModule valImport, ident) val
--                                              )
--                                            ]
--                                    }
--                            )
--       `updateConcurrently` ( onImportMap P.importedTypeOps \opImport -> do
--                                let opName = P.disqualify $ P.importName opImport
--                                (aliasModName, alias) <- fromJustWithErr opName <$> selectTypeOperatorAlias conn (P.importSourceModule opImport) opName
--                                synMb <- selectTypeSynonym' conn aliasModName alias
--                                case synMb of
--                                  Just syn -> do
--                                    pure $ \env' ->
--                                      env'
--                                        { E.typeSynonyms =
--                                            E.typeSynonyms env'
--                                              <> Map.fromList
--                                                [ (P.Qualified (P.ByModuleName (P.importSourceModule opImport)) alias, syn),
--                                                  (P.Qualified (P.ByModuleName aliasModName) alias, syn)
--                                                ]
--                                        }
--                                  Nothing -> do
--                                    type' <- selectType' conn aliasModName alias
--                                    pure $ \env' ->
--                                      env'
--                                        { E.types =
--                                            E.types env'
--                                              <> Map.fromList
--                                                [ (P.Qualified (P.ByModuleName (P.importSourceModule opImport)) alias, fromJustWithErr opName type'),
--                                                  (P.Qualified (P.ByModuleName aliasModName) alias, fromJustWithErr opName type')
--                                                ]
--                                        }
--                            )
--       `updateConcurrently` ( onImportMap P.importedValueOps \opImport -> do
--                                let opName = P.disqualify $ P.importName opImport
--                                (aliasModName, alias) <- fromJustWithErr opName <$> selectValueOperatorAlias conn (P.importSourceModule opImport) opName
--                                if isUpper $ T.head alias
--                                  then do
--                                    let ctrName = P.ProperName alias
--                                        qual = P.Qualified (P.ByModuleName aliasModName) ctrName
--                                    val <- selectDataConstructor conn qual
--                                    pure $ \env' ->
--                                      env'
--                                        { E.dataConstructors =
--                                            E.dataConstructors env'
--                                              <> Map.fromList [(qual, fromJustWithErr qual val)]
--                                        }
--                                  else do
--                                    let ident = P.Ident alias
--                                        qual = P.Qualified (P.ByModuleName aliasModName) ident
--                                    val <- selectEnvValue conn qual
--                                    pure $ \env' ->
--                                      env'
--                                        { E.names =
--                                            E.names env'
--                                              <> Map.fromList [(qual, fromJustWithErr qual val)]
--                                        }
--                            )

--   let env = importFn E.initEnvironment

--   envConstraintFns <- forConcurrently (getEnvConstraints env) \c -> do
--     let (classMod, className) = toDbQualifer $ constraintClass c
--     importClass' classMod classMod className

--   pure $ foldl' (&) env envConstraintFns
--   where
--     -- importName :: P.ModuleName -> P.Name -> IO (E.Environment -> E.Environment)
--     -- importName mName name = _ importRef mName $  getImportSrc mName name
--     imports :: P.Imports
--     imports = lookupImports modName exportEnv

--     onImportMap ::
--       ( P.Imports ->
--         Map
--           (P.Qualified a)
--           [P.ImportRecord a]
--       ) ->
--       ( P.ImportRecord a ->
--         IO (P.Environment -> P.Environment)
--       ) ->
--       IO (P.Environment -> P.Environment)
--     onImportMap getImports fn =
--       pipe <$> forConcurrently (Map.toList $ getImports imports) \(_, recs) ->
--         pipe <$> forConcurrently recs fn'
--       where
--         fn' ir = if P.importSourceModule ir == modName then pure identity else fn ir

--     -- importValue :: P.ModuleName -> P.Qualified P.Ident -> IO (E.Environment -> E.Environment)
--     -- importValue mName = \case
--     --   P.Qualified _ ident -> do
--     --     let qual = P.Qualified (P.ByModuleName mName) ident
--     --     val <- selectEnvValue conn qual
--     --     pure $ \env' -> env' {E.names = E.names env' <> Map.fromList [(qual, fromJustWithErr qual val)]}
--     --   where
--     --     exports = lookupExports mName exportEnv

--     importClass modName' qual className = do
--       typeClass <- fromJust <$> selectTypeClass conn modName' className
--       let dictName = P.Qualified (P.ByModuleName modName') . P.dictTypeName . coerceProperName $ className
--           typeQual = P.Qualified (P.ByModuleName modName') $ coerceProperName className
--       type' <- selectType conn typeQual
--       dictVal <- selectType conn dictName

--       let ctrMb :: Maybe (P.Qualified (P.ProperName 'P.ConstructorName))
--           ctrMb =
--             P.Qualified (P.ByModuleName modName') <$> case dictVal of
--               Just (_, P.DataType _ _ [(ctr', _)]) -> Just ctr'
--               _ -> Nothing

--       ctrData <- ctrMb & maybe (pure Nothing) (selectDataConstructor conn)
--       instances <- selectClassInstancesByClassName conn $ P.Qualified (P.ByModuleName modName') className

--       superClassImports <- forConcurrently (typeClassSuperclasses typeClass) \super -> case P.constraintClass super of
--         P.Qualified (P.ByModuleName superModName) superClassName -> do
--           -- TODO add check for existing class in env
--           importClass superModName (P.Qualified (P.ByModuleName superModName) superClassName) superClassName
--         _ -> pure identity

--       pure $
--         pipe superClassImports
--           >>> \env' ->
--             env'
--               { E.typeClasses =
--                   E.typeClasses env'
--                     <> Map.fromList
--                       [ (qual, typeClass),
--                         (P.Qualified (P.ByModuleName modName') className, typeClass)
--                       ],
--                 E.types =
--                   E.types env'
--                     <> Map.fromList
--                       ( [ (typeQual, fromJust type')
--                         ]
--                           <> case dictVal of
--                             Just val -> [(dictName, val)]
--                             _ -> []
--                       ),
--                 E.dataConstructors =
--                   E.dataConstructors env' <> Map.fromList case (ctrMb, ctrData) of
--                     (Just ctr', Just ctrData') -> [(ctr', ctrData')]
--                     _ -> [],
--                 E.typeClassDictionaries = P.addDictsToEnvMap instances (E.typeClassDictionaries env')
--               }
--     importName :: P.ModuleName -> P.Qualified P.Name -> IO (E.Environment -> E.Environment)
--     importName mName (P.Qualified (P.ByModuleName _) name) = do
--       -- when (modName' /= mName) do
--       --   putErrText $ "importName called with different module names: " <> show modName' <> " and " <> show mName
--       --   putErrText $ "name: " <> show name
--       case name of
--         P.IdentName ident -> do
--           let P.ExportSource {..} = fromJustWithErr (mName, ident) $ Map.lookup ident (P.exportedValues exports)
--               qual = P.Qualified (P.ByModuleName exportSourceDefinedIn) ident
--           val <- selectEnvValue conn qual
--           let importedModuleName = getImportedModule mName ident $ P.importedValues imports
--           pure $ \env' -> env' {E.names = E.names env' <> Map.fromList [(P.Qualified (P.ByModuleName importedModuleName) ident, fromJustWithErr ident val)]}
--         P.ValOpName opName -> do
--           let P.ExportSource {..} = fromJustWithErr (mName, opName) $ Map.lookup opName (P.exportedValueOps exports)
--           (aliasModName, alias) <- fromJustWithErr (mName, opName) <$> selectValueOperatorAlias conn exportSourceDefinedIn opName
--           if isUpper $ T.head alias
--             then do
--               let ctrName = P.ProperName alias
--                   qual = P.Qualified (P.ByModuleName aliasModName) ctrName
--               val <- selectDataConstructor conn qual
--               pure $ \env' ->
--                 env'
--                   { E.dataConstructors =
--                       E.dataConstructors env'
--                         <> Map.fromList [(qual, fromJustWithErr qual val)]
--                   }
--             else do
--               let ident = P.Ident alias
--                   qual = P.Qualified (P.ByModuleName aliasModName) ident
--               val <- selectEnvValue conn qual
--               pure $ \env' ->
--                 env'
--                   { E.names =
--                       E.names env'
--                         <> Map.fromList [(qual, fromJustWithErr qual val)]
--                   }
--         P.TyName tyName -> do
--           let (_, P.ExportSource {..}) = fromJust $ Map.lookup tyName (P.exportedTypes exports)
--           let qual = P.Qualified (P.ByModuleName exportSourceDefinedIn) tyName
--           type' <- selectType conn qual
--           ctrVals <- selectTypeDataConstructors conn qual
--           let importedModuleName = getImportedModule mName tyName $ P.importedTypes imports
--           pure $ \env' ->
--             env'
--               { E.types = E.types env' <> Map.fromList [(P.Qualified (P.ByModuleName importedModuleName) tyName, fromJust type')],
--                 E.dataConstructors = E.dataConstructors env' <> Map.fromList ctrVals
--               }
--         P.TyOpName opName -> do
--           let P.ExportSource {..} = fromJust $ Map.lookup opName (P.exportedTypeOps exports)
--           (aliasModName, alias) <- fromJustWithErr (mName, opName) <$> selectTypeOperatorAlias conn exportSourceDefinedIn opName
--           let qual = P.Qualified (P.ByModuleName aliasModName) alias
--           val <- selectType conn qual
--           let importedModuleName = getImportedModule mName alias $ P.importedTypes imports
--           pure $ \env' ->
--             env'
--               { E.types = E.types env' <> Map.fromList [(P.Qualified (P.ByModuleName importedModuleName) alias, fromJustWithErr qual val)]
--               }
--         P.TyClassName className -> do
--           let P.ExportSource {..} = fromJust $ Map.lookup className (P.exportedTypeClasses exports)
--           importClass' mName exportSourceDefinedIn className
--         P.DctorName ctrName -> do
--           let containsCtr (_, (ctrs, _)) = ctrName `elem` ctrs
--               (_, (_, P.ExportSource {..})) = fromJust $ find containsCtr $ Map.toList $ P.exportedTypes exports -- Map.find ctrName (P.exportedDataConstructors exports)
--               qual = P.Qualified (P.ByModuleName exportSourceDefinedIn) ctrName
--           val <- selectDataConstructor conn qual
--           let importedModuleName = getImportedModule mName ctrName $ P.importedDataConstructors imports
--           pure $ \env' -> env' {E.dataConstructors = E.dataConstructors env' <> Map.fromList [(P.Qualified (P.ByModuleName importedModuleName) ctrName, fromJustWithErr ctrName val)]}
--         P.ModName _ -> internalError "importName called with ModName"
--       where
--         exports :: P.Exports
--         exports = lookupExports mName exportEnv
--     importName _ _ = pure identity

--     getImportedModule ::
--       (Ord a) =>
--       (Foldable f) =>
--       P.ModuleName ->
--       a ->
--       Map (P.Qualified a) (f (P.ImportRecord a)) ->
--       P.ModuleName
--     getImportedModule mName ident imports' = fromMaybe mName do
--       importRecs <- Map.lookup (P.Qualified (P.ByModuleName mName) ident) imports'
--       importRec <- head importRecs
--       pure $ P.importSourceModule importRec
--     -- imports :: P.Imports
--     -- imports = lookupImports mName exportEnv

--     -- case
--     -- P.IdentName ident -> do
--     --   let qual = P.Qualified (P.ByModuleName mName) ident
--     --   val <- selectEnvValue conn qual
--     --   pure $ \env' -> env' {E.names = E.names env' <> Map.fromList [(qual, fromJustWithErr qual val)]}
--     -- P.ValOpName opName -> do
--     --   (aliasModName, alias) <- fromJustWithErr (mName, opName) <$> selectValueOperatorAlias conn mName opName
--     --   if isUpper $ T.head alias
--     --     then do
--     --       let qual = P.Qualified (P.ByModuleName aliasModName) (P.ProperName alias)
--     --       val <- selectDataConstructor conn qual
--     --       pure $ \env' -> env' {E.dataConstructors = E.dataConstructors env' <> Map.fromList [(qual, fromJustWithErr qual val)]}
--     --     else do
--     --       let qual = P.Qualified (P.ByModuleName aliasModName) (P.Ident alias)
--     --       val <- selectEnvValue conn qual
--     --       pure $ \env' -> env' {E.names = E.names env' <> Map.fromList [(qual, fromJustWithErr qual val)]}
--     -- P.TyName tyName -> do
--     --   let qual = P.Qualified (P.ByModuleName mName) tyName
--     --   type' <- selectType conn qual
--     --   ctrVals <- selectTypeDataConstructors conn qual
--     --   pure $ \env' ->
--     --     env'
--     --       { E.types = E.types env' <> Map.fromList [(qual, fromJustWithErr qual type')],
--     --         E.dataConstructors = E.dataConstructors env' <> Map.fromList ctrVals
--     --       }

--     -- P.TyOpName opName -> do
--     --   (aliasModName, alias) <- fromJustWithErr opName <$> selectTypeOperatorAlias conn mName opName
--     --   let qual = P.Qualified (P.ByModuleName aliasModName) alias
--     --   val <- selectType conn qual
--     --   pure $ \env' ->
--     --     env'
--     --       { E.types = E.types env' <> Map.fromList [(qual, fromJustWithErr qual val)]
--     --       }

--     -- P.DctorName dctorName -> do
--     --   let qual = P.Qualified (P.ByModuleName mName) dctorName
--     --   val <- selectDataConstructor conn qual
--     --   pure $ \env' -> env' {E.dataConstructors = E.dataConstructors env' <> Map.fromList [(qual, fromJustWithErr qual val)]}
--     -- P.TyClassName className -> do
--     --   importClass' mName className
--     -- P.ModName _ -> internalError "importName called with ModName"

--     -- where
--     --   exports = lookupExports mName exportEnv

--     -- importRef :: P.ModuleName -> P.DeclarationRef -> IO (E.Environment -> E.Environment)
--     -- importRef mName = \case
--     --   P.TypeClassRef _ className -> importClass' mName className
--     --   P.TypeRef _ tyName ctrs -> do
--     --     let qual = P.Qualified (P.ByModuleName mName) tyName
--     --     type' <- selectType conn qual
--     --     ctrVals <- case ctrs of
--     --       Nothing -> selectTypeDataConstructors conn qual
--     --       Just ctrs' -> forConcurrently ctrs' \ctr -> do
--     --         let qual' = P.Qualified (P.ByModuleName mName) ctr
--     --         val <- selectDataConstructor conn qual'
--     --         pure (qual', fromJustWithErr qual' val)
--     --     pure $ \env' ->
--     --       env'
--     --         { E.types = E.types env' <> Map.fromList [(qual, fromJust type')],
--     --           E.dataConstructors = E.dataConstructors env' <> Map.fromList ctrVals
--     --         }
--     --   P.ValueRef _ ident -> do
--     --     let qual = P.Qualified (P.ByModuleName mName) ident
--     --     val <- selectEnvValue conn qual
--     --     pure $ \env' -> env' {E.names = E.names env' <> Map.fromList [(qual, fromJustWithErr qual val)]}
--     --   P.TypeInstanceRef _ ident _ -> do
--     --     let qual = P.Qualified (P.ByModuleName mName) ident
--     --     val <- selectClassInstance conn qual
--     --     pure $ \env' -> env' {E.typeClassDictionaries = P.addDictsToEnvMap [fromJust val] (E.typeClassDictionaries env')}
--     --   P.ModuleRef _ _ -> internalError "importRef called with ModuleRef"
--     --   P.ReExportRef _ _ ref -> importRef mName ref
--     --   P.ValueOpRef _ opName -> do
--     --     (aliasModName, alias) <- fromJust <$> selectValueOperatorAlias conn mName opName
--     --     if isUpper $ T.head alias
--     --       then do
--     --         let qual = P.Qualified (P.ByModuleName aliasModName) (P.ProperName alias)
--     --         val <- selectDataConstructor conn qual
--     --         pure $ \env' -> env' {E.dataConstructors = E.dataConstructors env' <> Map.fromList [(qual, fromJustWithErr qual val)]}
--     --       else do
--     --         let qual = P.Qualified (P.ByModuleName aliasModName) (P.Ident alias)
--     --         val <- selectEnvValue conn qual
--     --         pure $ \env' -> env' {E.names = E.names env' <> Map.fromList [(qual, fromJustWithErr qual val)]}
--     --   P.TypeOpRef _ opName -> do
--     --     (aliasModName, alias) <- fromJustWithErr opName <$> selectTypeOperatorAlias conn mName opName
--     --     let qual = P.Qualified (P.ByModuleName aliasModName) alias
--     --     val <- selectType conn qual
--     --     pure $ \env' ->
--     --       env'
--     --         { E.types = E.types env' <> Map.fromList [(qual, fromJustWithErr qual val)]
--     --         }

--     -- importModule mName = importModuleHiding mName []

--     -- importModuleHiding mName hideRefs = do
--     --   allRefs <- selectModuleExports conn mName
--     --   let refs = filter (not . flip Set.member hiddenRefSet) allRefs
--     --   importRefs mName refs
--     --   where
--     --     hiddenRefSet = Set.fromList hideRefs

--     importClass' :: P.ModuleName -> P.ModuleName -> P.ProperName 'P.ClassName -> IO (E.Environment -> E.Environment)
--     importClass' mName _modDefinedIn className = do
--       when (mName /= _modDefinedIn) do
--         putErrText $ "importClass' called with different module names: " <> show mName <> " and " <> show _modDefinedIn
--         putErrText $ "className: " <> show className

--       let qual = P.Qualified (P.ByModuleName mName) className
--           typeQual = P.Qualified (P.ByModuleName mName) $ coerceProperName className
--       type' <- selectType conn typeQual
--       typeClass <- fromJust <$> selectTypeClass conn mName className
--       let dictName = P.Qualified (P.ByModuleName mName) . P.dictTypeName . coerceProperName $ className
--       dictVal <- selectType conn dictName

--       let ctrMb :: Maybe (P.Qualified (P.ProperName 'P.ConstructorName))
--           ctrMb =
--             P.Qualified (P.ByModuleName mName) <$> case dictVal of
--               Just (_, P.DataType _ _ [(ctr', _)]) -> Just ctr'
--               _ -> Nothing

--       ctrData <- ctrMb & maybe (pure Nothing) (selectDataConstructor conn)
--       superClassImports <- forConcurrently (typeClassSuperclasses typeClass) \super -> case P.constraintClass super of
--         P.Qualified (P.ByModuleName superModName) superClassName -> do
--           importClass' superModName superModName superClassName
--         _ -> pure identity

--       instances <- selectClassInstancesByClassName conn qual

--       pure $
--         pipe superClassImports >>> \env' ->
--           env'
--             { E.typeClasses = E.typeClasses env' <> Map.fromList [(qual, typeClass)],
--               E.types =
--                 E.types env'
--                   <> Map.fromList
--                     ( [ (typeQual, fromJust type')
--                       ]
--                         <> case dictVal of
--                           Just val -> [(dictName, val)]
--                           _ -> []
--                     ),
--               E.dataConstructors =
--                 E.dataConstructors env'
--                   <> Map.fromList case (ctrMb, ctrData) of
--                     (Just ctr', Just ctrData') -> [(ctr', ctrData')]
--                     _ -> [],
--               E.typeClassDictionaries = P.addDictsToEnvMap instances (E.typeClassDictionaries env')
--             }

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

-- pipeSet = pipe . Set.toList

updateConcurrently :: IO (a -> b) -> IO (b -> c) -> IO (a -> c)
updateConcurrently a b = do
  f <- a
  g <- b
  pure $ f >>> g



-- updateConcurrently :: IO (a -> b) -> IO (b -> c) -> IO (a -> c)
-- updateConcurrently a b = do
--   (f, g) <- concurrently a b
--   pure $ f >>> g

-- xx = [TypeConstructor    (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"}))]

-- xx = [TypeApp    (TypeConstructor    (Qualified (ByModuleName (ModuleName "Type.Proxy")) (ProperName {runProperName = "Proxy"}))) (TypeVar    "a")]
-- xx = [TypeConstructor    (Qualified (ByModuleName (ModuleName "Data.Unit")) (ProperName {runProperName = "Unit"}))]
-- xx = [TypeConstructor    (Qualified (ByModuleName (ModuleName "Prim.RowList")) (ProperName {runProperName = "Nil"})),TypeVar    "row",REmpty
-- xx = [TypeApp    (TypeApp    (TypeApp    (TypeConstructor    (Qualified (ByModuleName (ModuleName "Prim.RowList")) (ProperName {runProperName = "Cons"}))) (TypeVar    "key")) (TypeVar    "focus")) (TypeVar    "rowlistTail"),TypeVar    "row",TypeVar    [TypeApp    (TypeApp    (TypeConstructor    (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeVar    "a")) (TypeVar    "b")]
-- xx = [TypeApp    (TypeConstructor    (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Record"}))) (TypeVar    "row")]

class GetEnv m where 
  getName :: P.Qualified P.Ident -> m (Maybe (P.SourceType, P.NameKind, P.NameVisibility))
  getType :: P.Qualified (P.ProperName 'P.TypeName) -> m (Maybe (P.SourceType, P.TypeKind))
  getDataConstructor :: P.Qualified (P.ProperName 'P.ConstructorName) -> m (Maybe (P.DataDeclType, P.ProperName 'P.TypeName, P.SourceType, [P.Ident]))
  getTypeSynonym :: P.Qualified (P.ProperName 'P.TypeName) -> m (Maybe ([(Text, Maybe P.SourceType)], P.SourceType))
  getTypeClass :: P.Qualified (P.ProperName 'P.ClassName) -> m (Maybe P.TypeClassData)
  getTypeClassDictionaries :: m [NamedDict]
  getTypeClassDictionary :: P.Qualified (P.ProperName 'P.ClassName) -> m (Map.Map (P.Qualified P.Ident) (NEL.NonEmpty P.NamedDict))
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
    liftIO $ key <$> selectClassInstancesByClassName conn cls
    where 
      key = Map.fromListWith (<>) . fmap (\d -> (P.tcdValue d, pure d))

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
  getTypeClassDictionary _ = pure Map.empty
  deleteModuleEnv _ = pure ()