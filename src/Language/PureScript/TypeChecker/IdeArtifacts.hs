-- | Stores information about the source code that is useful for the IDE
-- | This includes value types and source spans
module Language.PureScript.TypeChecker.IdeArtifacts
  ( IdeArtifacts,
    IdeArtifact (..),
    IdeArtifactValue (..),
    getArtifactsAtPosition,
    emptyIdeArtifacts,
    insertIaExpr,
    insertIaBinder,
    insertIaDecl,
    insertIaType,
    insertIaIdent,
    smallestArtifact,
    debugIdeArtifacts,
    insertIaTypeName,
    insertIaClassName,
    moduleNameFromQual,
  )
where

-- import Language.PureScript qualified as P

import Data.Map qualified as Map
import Data.Text qualified as T
import Language.PureScript.AST.Binders qualified as P
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.AST.SourcePos qualified as P
import Language.PureScript.Names qualified as P
import Language.PureScript.Types qualified as P
import Protolude
import Safe (minimumByMay)

data IdeArtifacts = IdeArtifacts (Map Line [IdeArtifact]) deriving (Show)

type Line = Int

emptyIdeArtifacts :: IdeArtifacts
emptyIdeArtifacts = IdeArtifacts Map.empty

debugIdeArtifacts :: IdeArtifacts -> Text
debugIdeArtifacts = T.intercalate "\n" . fmap showCount . lineCounts
  where
    showCount :: (Int, Int) -> Text
    showCount (line, count) = show line <> ": " <> show count
    lineCounts :: IdeArtifacts -> [(Int, Int)]
    lineCounts (IdeArtifacts m) = Map.toList m <&> fmap length

data IdeArtifact = IdeArtifact
  { iaSpan :: P.SourceSpan,
    iaValue :: IdeArtifactValue,
    iaType :: P.SourceType,
    iaDefinitionModule :: Maybe P.ModuleName,
    iaDefinitionPos :: Maybe (Either P.SourcePos P.SourceSpan)
  }
  deriving (Show)

data IdeArtifactValue
  = IaExpr P.Expr
  | IaDecl P.Declaration
  | IaBinder P.Binder
  | IaIdent Text
  | IaType P.SourceType
  | IaTypeName (P.ProperName 'P.TypeName)
  | IaClassName (P.ProperName 'P.ClassName)
  deriving (Show)

-- valueCtr :: IdeArtifactValue -> Text
-- valueCtr = \case
--   IaExpr {} -> "Expr"
--   IaDecl {} -> "Decl"
--   IaBinder {} -> "Binder"
--   IaIdent {} -> "BinderIdent"
--   IaType {} -> "Type"

smallestArtifact :: [IdeArtifact] -> Maybe IdeArtifact
smallestArtifact = minimumByMay (compare `on` (\a -> (artifactSize a, negate $ artifactInterest a)))

artifactSize :: IdeArtifact -> (Int, Int)
artifactSize (IdeArtifact {..}) =
  ( P.sourcePosLine (P.spanEnd iaSpan) - P.sourcePosLine (P.spanStart iaSpan),
    P.sourcePosColumn (P.spanEnd iaSpan) - P.sourcePosColumn (P.spanStart iaSpan)
  )


-- | Prioritize artifacts that are more likely to be interesting to the developer on hover or click
artifactInterest :: IdeArtifact -> Int
artifactInterest (IdeArtifact {..}) = case iaValue of
  IaBinder {} -> 1
  IaTypeName {} -> 1
  IaClassName {} -> 1
  _ -> 0

getArtifactsAtPosition :: P.SourcePos -> IdeArtifacts -> [IdeArtifact]
getArtifactsAtPosition pos (IdeArtifacts m) =
  Map.lookup (P.sourcePosLine pos) m
    & fromMaybe []
    & filter (\ia -> P.sourcePosColumn (P.spanStart (iaSpan ia)) <= posCol && P.sourcePosColumn (P.spanEnd (iaSpan ia)) >= posCol)
  where
    posCol = P.sourcePosColumn pos

insertIaExpr :: P.Expr -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaExpr expr ty = case ss of
  Just span -> insertAtLines span (IaExpr expr) ty mName defSpan
  Nothing -> identity
  where
    ss = P.exprSourceSpan expr
    defSpan =
      Left <$> case expr of
        P.Var _ q -> posFromQual q
        P.Constructor _ q -> posFromQual q
        P.Op _ q -> posFromQual q
        _ -> Nothing

    mName = case expr of
      P.Var _ q -> moduleNameFromQual q
      P.Constructor _ q -> moduleNameFromQual q
      P.Op _ q -> moduleNameFromQual q
      _ -> Nothing

insertIaIdent :: P.SourceSpan -> P.Ident -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaIdent ss ident ty = case ident of
  P.Ident ident' -> insertAtLines ss (IaIdent ident') ty Nothing (Just $ Right ss)
  _ -> identity

insertIaBinder :: P.Binder -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaBinder binder ty = case binderSourceSpan binder of
  Just ss -> insertAtLines ss (IaBinder binder) ty Nothing (Just $ Right ss)
  Nothing -> identity

insertIaDecl :: P.Declaration -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaDecl decl ty = insertAtLines (P.declSourceSpan decl) (IaDecl decl) ty Nothing Nothing

insertIaType :: P.SourceType -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaType ty kind = insertAtLines (fst $ P.getAnnForType ty) (IaType ty) kind Nothing Nothing

insertIaTypeName :: P.SourceSpan -> P.ProperName 'P.TypeName -> Maybe P.ModuleName -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaTypeName ss name mName kind = insertAtLines ss (IaTypeName name) kind mName (Just $ Right $ fst $ P.getAnnForType kind)

insertIaClassName :: P.SourceSpan -> P.ProperName 'P.ClassName -> Maybe P.ModuleName -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaClassName ss name mName kind = insertAtLines ss (IaClassName name) kind mName (Just $ Right $ fst $ P.getAnnForType kind)

binderSourceSpan :: P.Binder -> Maybe P.SourceSpan
binderSourceSpan = \case
  P.NullBinder -> Nothing
  P.LiteralBinder ss _ -> Just ss
  P.VarBinder ss _ -> Just ss
  P.ConstructorBinder ss _ _ -> Just ss
  P.NamedBinder ss _ _ -> Just ss
  P.PositionedBinder ss _ _ -> Just ss
  P.TypedBinder _ b -> binderSourceSpan b
  P.OpBinder ss _ -> Just ss
  P.BinaryNoParensBinder {} -> Nothing
  P.ParensInBinder {} -> Nothing

posFromQual :: P.Qualified a -> Maybe P.SourcePos
posFromQual (P.Qualified (P.BySourcePos pos) _) = Just pos
posFromQual _ = Nothing

moduleNameFromQual :: P.Qualified a -> Maybe P.ModuleName
moduleNameFromQual (P.Qualified (P.ByModuleName mn) _) = Just mn
moduleNameFromQual _ = Nothing

insertAtLines :: P.SourceSpan -> IdeArtifactValue -> P.SourceType -> Maybe P.ModuleName -> Maybe (Either P.SourcePos P.SourceSpan) -> IdeArtifacts -> IdeArtifacts
insertAtLines span value ty mName defSpan (IdeArtifacts m) = IdeArtifacts $ foldr insert m (linesFromSpan span)
  where
    insert line = Map.insertWith (<>) line [IdeArtifact span value ty mName defSpan]

linesFromSpan :: P.SourceSpan -> [Line]
linesFromSpan ss = [P.sourcePosLine $ P.spanStart ss .. P.sourcePosLine $ P.spanEnd ss]

-- insertIaExpr :: P.SourceSpan -> P.Expr -> P.SourceType -> IdeArtifacts -> IdeArtifacts
-- insertIaExpr ann expr ty (IdeArtifacts m) = IdeArtifacts $ Map.insert line (ann, IaExpr expr, ty) m