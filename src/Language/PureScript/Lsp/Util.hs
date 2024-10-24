{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

module Language.PureScript.Lsp.Util where

import Codec.Serialise qualified as S
-- import Language.PureScript.Linter qualified as P

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
import Language.PureScript.Externs qualified as P
import Language.PureScript.Names qualified as P
import Language.PureScript.Types qualified as P
import Protolude hiding (to)

posInSpan :: Types.Position -> AST.SourceSpan -> Bool
posInSpan (Types.Position line col) (AST.SourceSpan _ (AST.SourcePos startLine startCol) (AST.SourcePos endLine endCol)) =
  not (startLine == 1 && startCol == 1)  -- ignore generated spans
    && startLine <= atLine
    && endLine >= atLine
    && startCol <= atCol
    && endCol >= atCol
  where
    atLine = fromIntegral line + 1
    atCol = fromIntegral col + 1

posInSpanLines :: Types.Position -> AST.SourceSpan -> Bool
posInSpanLines (Types.Position line _) (AST.SourceSpan _ (AST.SourcePos startLine _) (AST.SourcePos endLine _)) =
  startLine <= fromIntegral (line + 1)
    && endLine >= fromIntegral (line + 1)

getDeclarationAtPos :: Types.Position -> [P.Declaration] -> Maybe P.Declaration
getDeclarationAtPos pos = find (posInSpan pos . fst . declSourceAnn)

getWordAt :: Rope -> Types.Position -> (Types.Range, Text)
getWordAt = getByPredAt isWordBreak

isWordBreak :: Char -> Bool
isWordBreak = not . (isAlphaNum ||^ (== '_') ||^ (== '.'))

getSymbolAt :: Rope -> Types.Position -> (Types.Range, Text)
getSymbolAt = getByPredAt isSymbolBreak

isSymbolBreak :: Char -> Bool
isSymbolBreak = isSpace ||^ (== '(') ||^ (== ')') ||^ (== '{') ||^ (== '}') ||^ (== '[') ||^ (== ']') ||^ (== ',')

getByPredAt :: (Char -> Bool) -> Rope -> Types.Position -> (Types.Range, Text)
getByPredAt charPred file pos@(Types.Position {..}) =
  if Rope.lengthInLines file < fromIntegral _line || _line < 0
    then (Types.Range pos pos, "")
    else
      let (_, after) = splitAtLine (fromIntegral _line) file
          (ropeLine, _) = splitAtLine 1 after
          line' = Rope.toText ropeLine
          (wordStartCol, wordEndCol, _word) = getOnLine charPred line' _character
       in (Types.Range (Types.Position _line $ fromIntegral wordStartCol) (Types.Position _line $ fromIntegral wordEndCol), _word)

getOnLine :: (Char -> Bool) -> Text -> UInt -> (Int, Int, Text)
getOnLine charPred line' col =
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
      ch | charPred ch -> idx
      _ -> getNextWs (idx + 1) txt

    getPrevWs :: Int -> Text -> Int
    getPrevWs 0 _ = 0
    getPrevWs idx txt = case T.index txt idx of
      ch | charPred ch -> idx + 1
      _ -> getPrevWs (idx - 1) txt

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

declsAtLine :: Int -> [P.Declaration] -> [P.Declaration]
declsAtLine l = go . sortBy (comparing declStartLine)
  where
    go (d : ds) | declStartLine d <= l && declEndLine d >= l = d : go ds
    go (d : d' : ds)
      | declStartLine d <= l && declStartLine d' > l && unsureEndLine d = d : go (d' : ds)
      | otherwise = go (d' : ds)
    go [d] | declStartLine d <= l = [d]
    go _ = []

    unsureEndLine = \case
      P.ValueDeclaration {} -> True
      P.ExternDeclaration {} -> True
      P.TypeClassDeclaration {} -> True
      P.TypeInstanceDeclaration {} -> True
      _ -> False

declStartLine :: P.Declaration -> Int
declStartLine = P.sourcePosLine . AST.spanStart . P.declSourceSpan

declEndLine :: P.Declaration -> Int
declEndLine = P.sourcePosLine . AST.spanEnd . P.declSourceSpan

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

getOperatorValueName :: P.Declaration -> Maybe (P.Qualified P.Name)
getOperatorValueName = \case
  P.FixityDeclaration _ (Left (P.ValueFixity _ n _)) -> Just (either P.IdentName P.DctorName <$> n)
  P.FixityDeclaration _ (Right (P.TypeFixity _ n _)) -> Just (P.TyName <$> n)
  _ -> Nothing