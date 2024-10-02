{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

module Language.PureScript.Lsp.Util where

import Codec.Serialise qualified as S
-- import Language.PureScript.Linter qualified as P

import Control.Lens (Field1 (_1), Field2 (_2), Field3 (_3), view, (^.))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Utf16.Rope.Mixed as Rope
import Database.SQLite.Simple.ToField (ToField (toField))
import Language.LSP.Protocol.Types (UInt)
import Language.LSP.Protocol.Types qualified as Types
import Language.PureScript qualified as P
import Language.PureScript.AST.Declarations (declSourceAnn)
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationsAtSrcPos)
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.State (cachedRebuild)
import Language.PureScript.Lsp.Types (CurrentFile (currentEnv), LspEnvironment)
import Language.PureScript.Sugar.BindingGroups (usedTypeNames)
import Protolude hiding (to)
import "monad-logger" Control.Monad.Logger (MonadLogger, logDebugN)

posInSpan :: Types.Position -> Errors.SourceSpan -> Bool
posInSpan (Types.Position line col) (Errors.SourceSpan _ (Errors.SourcePos startLine startCol) (Errors.SourcePos endLine endCol)) =
  startLine <= fromIntegral (line + 1)
    && endLine >= fromIntegral (line + 1)
    && startCol <= fromIntegral (col + 1)
    && endCol >= fromIntegral (col + 1)

getDeclarationAtPos :: Types.Position -> [P.Declaration] -> Maybe P.Declaration
getDeclarationAtPos pos = find (posInSpan pos . fst . declSourceAnn)

getWordAt :: Rope -> Types.Position -> Text
getWordAt file Types.Position {..} =
  let (_, after) = splitAtLine (fromIntegral _line) file
      (ropeLine, _) = splitAtLine 1 after
      line' = Rope.toText ropeLine
   in getWordOnLine line' _character

getWordOnLine :: Text -> UInt -> Text
getWordOnLine line' col =
  let start = getPrevWs (fromIntegral col) line'
      end = getNextWs (fromIntegral col) line'
   in T.strip $ T.take (end - start) $ T.drop start line'
  where
    getNextWs :: Int -> Text -> Int
    getNextWs idx txt | idx >= T.length txt = idx
    getNextWs idx txt = case T.index txt idx of
      ch | isWordBreak ch -> idx
      _ -> getNextWs (idx + 1) txt

    getPrevWs :: Int -> Text -> Int
    getPrevWs 0 _ = 0
    getPrevWs idx txt = case T.index txt idx of
      ch | isWordBreak ch -> idx + 1
      _ -> getPrevWs (idx - 1) txt

    isWordBreak :: Char -> Bool
    isWordBreak = not . (isAlphaNum ||^ (== '_'))

getNamesAtPosition :: (MonadIO m, MonadLogger m, MonadReader LspEnvironment m) => Types.Position -> P.ModuleName -> Rope -> m (Set (P.Qualified P.Name))
getNamesAtPosition pos modName src = do
  let search = getWordAt src pos
  decls <- getAstDeclarationsAtSrcPos modName (positionToSourcePos pos)
  case head decls of
    Nothing -> do
      logDebugN $ "No declaration found at position " <> show pos
      pure mempty
    Just decl -> do
      logDebugN $ "Found declaration: " <> show decl
      let goDef _ = mempty
          getDeclName :: P.Declaration -> Set (P.Qualified P.Name)
          getDeclName decl' = case decl' of
            P.DataDeclaration _ _ n _ _ | True -> Set.singleton $ flip P.mkQualified modName $ P.TyName n
            P.TypeSynonymDeclaration _ n _ _ | True -> Set.singleton $ flip P.mkQualified modName $ P.TyName n
            P.TypeClassDeclaration _ n _ _ _ _ | True -> Set.singleton $ flip P.mkQualified modName $ P.TyClassName n
            P.TypeDeclaration (P.TypeDeclarationData _ _ st) -> Set.fromList $ getTypeNames st
            P.ValueDeclaration (P.ValueDeclarationData _ ident _ _ _) -> Set.singleton $ flip P.mkQualified modName $ P.IdentName ident
            P.ExternDeclaration _ _ st -> Set.fromList $ getTypeNames st
            P.ExternDataDeclaration _ name st -> Set.fromList (getTypeNames st) <> Set.singleton (flip P.mkQualified modName $ P.TyName name)
            _ -> mempty
          getExprName :: P.Expr -> Set (P.Qualified P.Name)
          getExprName expr = case expr of
            P.Var _ (P.Qualified qb ident) | True -> Set.singleton $ P.Qualified qb $ P.IdentName ident
            P.Constructor _ (P.Qualified qb ident) -> Set.singleton $ P.Qualified qb $ P.DctorName ident
            P.TypeClassDictionary (P.Constraint _ (P.Qualified qb ident) _ _ _) _ _ -> Set.singleton $ P.Qualified qb $ P.TyClassName ident
            P.DeferredDictionary (P.Qualified qb ident) _ -> Set.singleton $ P.Qualified qb $ P.TyClassName ident
            P.DerivedInstancePlaceholder (P.Qualified qb ident) _ -> Set.singleton $ P.Qualified qb $ P.TyClassName ident
            P.TypedValue _ _ tipe -> Set.fromList (getTypeNames tipe)
            _ -> mempty

          getTypeNames :: P.SourceType -> [P.Qualified P.Name]
          getTypeNames = P.everythingOnTypes (<>) goType
            where
              goType :: P.SourceType -> [P.Qualified P.Name]
              goType = \case
                P.TypeConstructor _ (P.Qualified _ pn) -> [flip P.mkQualified modName $ P.TyName pn]
                P.ConstrainedType _ (P.Constraint {..}) _ -> [fmap P.TyClassName constraintClass]
                -- P.TypeClassDictionary (P.Constraint {..}) _ _ -> [_ constraintClass]
                _ -> []

          goBinder :: P.Binder -> Set (P.Qualified P.Name)
          goBinder = \case
            P.ConstructorBinder _ (P.Qualified qb ident) _ -> Set.singleton $ P.Qualified qb $ P.DctorName ident
            P.OpBinder _ (P.Qualified qb ident) -> Set.singleton $ P.Qualified qb $ P.ValOpName ident
            P.TypedBinder st _ -> Set.fromList $ getTypeNames st
            _ -> mempty

          exprNames = P.everythingOnValues (<>) getDeclName getExprName goBinder goDef goDef ^. _1 $ decl
          typeNames = Set.fromList $ usedTypeNames modName decl
      pure $
        Set.filter ((==) search . printName . P.disqualify) $
          exprNames <> Set.map (flip P.mkQualified modName . P.TyName) typeNames

lookupTypeInEnv :: (MonadReader LspEnvironment m, MonadLogger m, MonadIO m) => P.Qualified P.Name -> m (Maybe P.SourceType)
lookupTypeInEnv (P.Qualified qb name) = do
  envMb :: Maybe P.Environment <- fmap currentEnv <$> cachedRebuild
  logDebugN $ "Looking up " <> show name <> " in environment"
  -- logDebugN $ "Environment: " <> show envMb
  pure $
    envMb
      >>= ( \(P.Environment {..}) -> case name of
              P.IdentName ident -> view _1 <$> Map.lookup (P.Qualified qb ident) names
              P.ValOpName _opName -> Nothing
              P.TyName tyName ->
                (view _1 <$> Map.lookup (P.Qualified qb tyName) types)
                  <|> (view _2 <$> Map.lookup (P.Qualified qb tyName) typeSynonyms)
              P.TyOpName _opName -> Nothing
              P.DctorName dctorName -> view _3 <$> Map.lookup (P.Qualified qb dctorName) dataConstructors
              P.TyClassName tyClassName ->
                (view _1 <$> Map.lookup (P.Qualified qb $ P.coerceProperName tyClassName) types)
              --  <|> (_ =<< Map.lookup (P.Qualified qb $ P.coerceProperName tyClassName) typeClasses)
              --  <|> (typeClassDictionaries)
              _ -> Nothing
              -- P.Qualified (P.ByModuleName mn) n -> P.lookupType n mn env
              -- P.Qualified (P.BySourcePos _) n -> P.lookupType n (P.moduleName env) env
          )

-- getNamesAtPosition :: (MonadIO m, MonadReader LspEnvironment m) => Types.Position -> P.ModuleName -> Rope -> m (Set (P.Qualified P.Name))
-- getNamesAtPosition pos modName src = do
--   let search = getWordAt src pos
--   cacheMb <- cachedRebuild
--   case getDeclarationAtPos pos =<< P.getModuleDeclarations . currentModule <$> cacheMb of
--     Nothing -> pure mempty
--     Just decl -> do
--       let goDef _ = mempty
--           getDeclName :: P.Declaration -> Set (P.Qualified P.Name)
--           getDeclName decl' = case decl' of
--             P.DataDeclaration _ _ n _ _ | P.runProperName n == search -> Set.singleton $ flip P.mkQualified modName $ P.TyName n
--             P.TypeSynonymDeclaration _ n _ _ | P.runProperName n == search -> Set.singleton $ flip P.mkQualified modName $ P.TyName n
--             P.TypeClassDeclaration _ n _ _ _ _ | P.runProperName n == search -> Set.singleton $ flip P.mkQualified modName $ P.TyClassName n
--             _ -> mempty
--           getExprName :: P.Expr -> Set (P.Qualified P.Name)
--           getExprName expr = case expr of
--             P.Var _ (P.Qualified qb ident) | runIdent ident == search -> Set.singleton $ P.Qualified qb $ P.IdentName ident
--             P.Constructor _ (P.Qualified qb ident) | P.runProperName ident == search -> Set.singleton $ P.Qualified qb $ P.DctorName ident
--             P.TypeClassDictionary (P.Constraint _ (P.Qualified qb ident) _ _ _) _ _ | P.runProperName ident == search -> Set.singleton $ P.Qualified qb $ P.TyClassName ident
--             _ -> mempty

--           exprNames = P.everythingOnValues (<>) getDeclName getExprName goDef goDef goDef ^. _1 $ decl
--           typeNames = Set.fromList $ filter ((==) search . P.runProperName) $ usedTypeNames modName decl
--       pure $ exprNames <> Set.map (flip P.mkQualified modName . P.TyName) typeNames

-- cacheMb
--   & maybe
--     (pure _)
--     \CurrentFile {..} -> do

--       -- let module' = P.efModule currentExterns
--       -- let decls = P.getModuleDeclarations module'
--       -- let file = P.efSource currentExterns
--       -- let word = getWordAt file Types.Position {..}
--       -- let decl = getDeclarationAtPos Types.Position {..} decls
--       -- let ident = P.Ident (P.IdentName $ P.Ident word)
--       -- pure $ P.IdentName ident
--       pure Nothing

data ExternsDeclarationCategory
  = EDCType
  | EDCTypeSynonym
  | EDCDataConstructor
  | EDCValue
  | EDCClass
  | EDCInstance
  deriving (Eq, Show, Read, Generic, S.Serialise)

instance ToField ExternsDeclarationCategory where
  toField = toField . S.serialise

efDeclCategory :: P.ExternsDeclaration -> ExternsDeclarationCategory
efDeclCategory = \case
  P.EDType {} -> EDCType
  P.EDTypeSynonym {} -> EDCTypeSynonym
  P.EDDataConstructor {} -> EDCDataConstructor
  P.EDValue {} -> EDCValue
  P.EDClass {} -> EDCClass
  P.EDInstance {} -> EDCInstance

efDeclSourceType :: P.ExternsDeclaration -> P.SourceType
efDeclSourceType = \case
  P.EDType _ ty _ -> ty
  P.EDTypeSynonym _ _ ty -> ty
  P.EDDataConstructor _ _ _ ty _ -> ty
  P.EDValue _ ty -> ty
  P.EDClass {} -> P.srcREmpty
  P.EDInstance {} -> P.srcREmpty

efDeclSourceSpan :: P.ExternsDeclaration -> P.SourceSpan
efDeclSourceSpan = \case
  P.EDClass _ _ _ _ _ _ -> P.nullSourceSpan
  P.EDInstance _ _ _ _ _ _ _ _ _ span -> span
  ed ->
    fromMaybe P.nullSourceSpan $ foldr (\(ss, _) _ -> Just ss) Nothing (efDeclSourceType ed)

efDeclComments :: P.ExternsDeclaration -> [P.Comment]
efDeclComments = foldr getComments [] . efDeclSourceType
  where
    getComments :: Errors.SourceAnn -> [P.Comment] -> [P.Comment]
    getComments (_, cs) acc = cs ++ acc
    
sourcePosToPosition :: Errors.SourcePos -> Types.Position
sourcePosToPosition (Errors.SourcePos line col) =
  Types.Position (fromIntegral $ line - 1) (fromIntegral $ col - 1)

positionToSourcePos :: Types.Position -> Errors.SourcePos
positionToSourcePos (Types.Position line col) =
  Errors.SourcePos (fromIntegral $ line + 1) (fromIntegral $ col + 1)