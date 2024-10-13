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
import Language.PureScript.AST qualified as AST
import Language.PureScript.AST qualified as P
import Language.PureScript.AST.Declarations (declSourceAnn)
-- import Language.PureScript.Sugar.BindingGroups (usedTypeNames)

import Language.PureScript.AST.SourcePos (widenSourceSpan)
import Language.PureScript.Comments qualified as P
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Linter qualified as P
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationsAtSrcPos)
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.State (cachedRebuild)
import Language.PureScript.Lsp.Types (LspEnvironment, OpenFile (ofFinalEnv))
import Language.PureScript.Names qualified as P
import Language.PureScript.Types qualified as P
import Protolude hiding (to)

posInSpan :: Types.Position -> AST.SourceSpan -> Bool
posInSpan (Types.Position line col) (AST.SourceSpan _ (AST.SourcePos startLine startCol) (AST.SourcePos endLine endCol)) =
  startLine <= fromIntegral (line + 1)
    && endLine >= fromIntegral (line + 1)
    && startCol <= fromIntegral (col + 1)
    && endCol >= fromIntegral (col + 1)

posInSpanLines :: Types.Position -> AST.SourceSpan -> Bool
posInSpanLines (Types.Position line _) (AST.SourceSpan _ (AST.SourcePos startLine _) (AST.SourcePos endLine _)) =
  startLine <= fromIntegral (line + 1)
    && endLine >= fromIntegral (line + 1)

getDeclarationAtPos :: Types.Position -> [P.Declaration] -> Maybe P.Declaration
getDeclarationAtPos pos = find (posInSpan pos . fst . declSourceAnn)

getWordAt :: Rope -> Types.Position -> (Types.Range, Text)
getWordAt file pos@(Types.Position {..}) =
  if Rope.lengthInLines file < fromIntegral _line || _line < 0
    then (Types.Range pos pos, "")
    else
      let (_, after) = splitAtLine (fromIntegral _line) file
          (ropeLine, _) = splitAtLine 1 after
          line' = Rope.toText ropeLine
          (wordStartCol, wordEndCol, _word) = getWordOnLine line' _character
       in (Types.Range (Types.Position _line $ fromIntegral wordStartCol) (Types.Position _line $ fromIntegral wordEndCol), _word)

getWordOnLine :: Text -> UInt -> (Int, Int, Text)
getWordOnLine line' col =
  if T.length line' < fromIntegral col || col < 0
    then (fromIntegral col, fromIntegral col, "")
    else
      let start = getPrevWs (fromIntegral col - 1) line'
          end = getNextWs (fromIntegral col) line'
       in (start, end, T.strip $ T.take (end - start) $ T.drop start line')
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
    isWordBreak = not . (isAlphaNum ||^ (== '_') ||^ (== '.'))

getNamesAtPosition :: (MonadIO m, MonadReader LspEnvironment m) => Types.Position -> P.ModuleName -> Rope -> m (Set (P.Qualified P.Name))
getNamesAtPosition pos moduleName' src = do
  let (_, search) = getWordAt src pos
  decls <- getAstDeclarationsAtSrcPos moduleName' (positionToSourcePos pos)
  pure $
    mconcat $
      decls <&> \decl -> do
        let goDef m _ = (m, mempty)
            getDeclName :: P.ModuleName -> P.Declaration -> (P.ModuleName, Set (P.Qualified P.Name))
            getDeclName modName decl' = case decl' of
              P.ImportDeclaration _ newMod _ _ -> (newMod, mempty)
              _ ->
                (modName,)
                  case decl' of
                    P.DataDeclaration _ _ n _ _ | True -> Set.singleton $ flip P.mkQualified modName $ P.TyName n
                    P.TypeSynonymDeclaration _ n _ _ | True -> Set.singleton $ flip P.mkQualified modName $ P.TyName n
                    P.TypeClassDeclaration _ n _ _ _ _ | True -> Set.singleton $ flip P.mkQualified modName $ P.TyClassName n
                    P.TypeDeclaration (P.TypeDeclarationData _ _ st) -> Set.fromList $ getTypeNames st
                    P.ValueDeclaration (P.ValueDeclarationData _ ident _ _ _) ->
                      Set.singleton $ flip P.mkQualified modName $ P.IdentName ident
                    P.ExternDeclaration _ _ st -> Set.fromList $ getTypeNames st
                    P.ExternDataDeclaration _ name st ->
                      Set.fromList (getTypeNames st)
                        <> Set.singleton (flip P.mkQualified modName $ P.TyName name)
                    _ -> mempty

            getExprNames :: P.ModuleName -> P.Expr -> (P.ModuleName, Set (P.Qualified P.Name))
            getExprNames modName expr = (modName,) case expr of
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
                  P.TypeConstructor _ ctr -> [fmap P.TyName ctr]
                  P.ConstrainedType _ (P.Constraint {..}) _ -> [fmap P.TyClassName constraintClass]
                  -- P.TypeClassDictionary (P.Constraint {..}) _ _ -> [_ constraintClass]
                  _ -> []

            goBinder :: P.ModuleName -> P.Binder -> (P.ModuleName, Set (P.Qualified P.Name))
            goBinder modName b = (modName,) case b of
              P.ConstructorBinder _ (P.Qualified qb ident) _ -> Set.singleton $ P.Qualified qb $ P.DctorName ident
              P.OpBinder _ (P.Qualified qb ident) -> Set.singleton $ P.Qualified qb $ P.ValOpName ident
              P.TypedBinder st _ -> Set.fromList $ getTypeNames st
              _ -> mempty

            exprNames = P.everythingWithContextOnValues moduleName' Set.empty (<>) getDeclName getExprNames goBinder goDef goDef ^. _1 $ decl
        -- typeNames = Set.fromList $ usedTypeNames moduleName' decl

        Set.filter ((==) search . printName . P.disqualify) exprNames

lookupTypeInEnv :: (MonadReader LspEnvironment m, MonadIO m) => FilePath -> P.Qualified P.Name -> m (Maybe P.SourceType)
lookupTypeInEnv fp (P.Qualified qb name) = do
  envMb :: Maybe P.Environment <- fmap ofFinalEnv <$> cachedRebuild fp
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
                view _1 <$> Map.lookup (P.Qualified qb $ P.coerceProperName tyClassName) types
              P.ModName _ -> Nothing
          )

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
    getComments :: AST.SourceAnn -> [P.Comment] -> [P.Comment]
    getComments (_, cs) acc = cs ++ acc

sourcePosToPosition :: AST.SourcePos -> Types.Position
sourcePosToPosition (AST.SourcePos line col) =
  Types.Position (fromIntegral $ line - 1) (fromIntegral $ col - 1)

positionToSourcePos :: Types.Position -> AST.SourcePos
positionToSourcePos (Types.Position line col) =
  AST.SourcePos (fromIntegral $ line + 1) (fromIntegral $ col + 1)

declToCompletionItemKind :: P.Declaration -> Maybe Types.CompletionItemKind
declToCompletionItemKind = \case
  P.DataDeclaration {} -> Just Types.CompletionItemKind_EnumMember
  P.TypeSynonymDeclaration {} -> Just Types.CompletionItemKind_Struct
  P.DataBindingGroupDeclaration {} -> Nothing
  P.TypeClassDeclaration {} -> Just Types.CompletionItemKind_Interface
  P.TypeDeclaration {} -> Just Types.CompletionItemKind_Class
  P.ValueDeclaration {} -> Just Types.CompletionItemKind_Value
  P.KindDeclaration {} -> Just Types.CompletionItemKind_Class
  P.RoleDeclaration {} -> Nothing
  P.ExternDeclaration {} -> Just Types.CompletionItemKind_Value
  _ -> Nothing

filePathToNormalizedUri :: FilePath -> Types.NormalizedUri
filePathToNormalizedUri = Types.toNormalizedUri . Types.filePathToUri

declSourceSpanWithExpr :: P.Declaration -> AST.SourceSpan
declSourceSpanWithExpr d = maybe span (widenSourceSpan span) exprSpan
  where
    span = P.declSourceSpan d
    exprSpan = case d of
      P.ValueDeclaration (P.ValueDeclarationData {..}) ->
        let go acc (P.GuardedExpr _ e) =
              case acc of
                Nothing -> findExprSourceSpan e
                Just acc' -> widenSourceSpan acc' <$> findExprSourceSpan e
         in foldl' go Nothing valdeclExpression
      _ -> Nothing

declAtLine :: Int -> [P.Declaration] -> Maybe P.Declaration
declAtLine l = go . sortBy (comparing declStartLine)
  where
    go (d : d' : ds)
      | declStartLine d <= l && declStartLine d' > l = Just d
      | otherwise = go (d' : ds)
    go [d] | declStartLine d >= l = Just d
    go _ = Nothing

declStartLine :: P.Declaration -> Int
declStartLine = P.sourcePosLine . AST.spanStart . P.declSourceSpan

findExprSourceSpan :: P.Expr -> Maybe AST.SourceSpan
findExprSourceSpan = goExpr
  where
    combine (Just a) _ = Just a
    combine _ b = b
    (_, goExpr, _, _, _) =
      P.everythingOnValues
        combine
        (Just . P.declSourceSpan)
        P.exprSourceSpan
        (const Nothing)
        (const Nothing)
        (const Nothing)