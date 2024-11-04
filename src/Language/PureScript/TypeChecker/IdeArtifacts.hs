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
    debugIdeArtifact,
    substituteArtifactTypes,
    endSubstitutions,
  )
where

-- import Language.PureScript qualified as P

import Data.Map qualified as Map
import Data.Text qualified as T
import Language.PureScript.AST.Binders qualified as P
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.AST.SourcePos qualified as P
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Names qualified as P
import Language.PureScript.Pretty.Types qualified as P
import Language.PureScript.Pretty.Values qualified as P
import Language.PureScript.Types qualified as P
import Protolude
import Safe (minimumByMay)
import Text.PrettyPrint.Boxes (render)

data IdeArtifacts
  = IdeArtifacts
      (Map Line [IdeArtifact]) -- with substitutions
      (Map Line [IdeArtifact]) -- without substitutions
  deriving (Show)

type Line = Int

emptyIdeArtifacts :: IdeArtifacts
emptyIdeArtifacts = IdeArtifacts Map.empty Map.empty

debugIdeArtifacts :: IdeArtifacts -> Text
debugIdeArtifacts = T.intercalate "\n" . fmap showCount . lineCounts
  where
    showCount :: (Int, Int) -> Text
    showCount (line, count) = show line <> ": " <> show count
    lineCounts :: IdeArtifacts -> [(Int, Int)]
    lineCounts (IdeArtifacts m _) = Map.toList m <&> fmap length

data IdeArtifact = IdeArtifact
  { iaSpan :: P.SourceSpan,
    iaValue :: IdeArtifactValue,
    iaType :: P.SourceType,
    iaDefinitionModule :: Maybe P.ModuleName,
    iaDefinitionPos :: Maybe (Either P.SourcePos P.SourceSpan)
  }
  deriving (Show)

data IdeArtifactValue
  = IaExpr Text P.Expr
  | IaDecl P.Declaration
  | IaBinder P.Binder
  | IaIdent Text
  | IaType P.SourceType
  | IaTypeName (P.ProperName 'P.TypeName)
  | IaClassName (P.ProperName 'P.ClassName)
  deriving (Show)

substituteArtifactTypes :: (P.SourceType -> P.SourceType) -> IdeArtifacts -> IdeArtifacts
substituteArtifactTypes f (IdeArtifacts m u) = IdeArtifacts m (Map.map (fmap (onArtifactType f)) u)

onArtifactType :: (P.SourceType -> P.SourceType) -> IdeArtifact -> IdeArtifact
onArtifactType f (IdeArtifact {..}) = IdeArtifact iaSpan iaValue (f iaType) iaDefinitionModule iaDefinitionPos

endSubstitutions :: IdeArtifacts -> IdeArtifacts
endSubstitutions (IdeArtifacts m u) = IdeArtifacts (Map.unionWith (<>) m u) Map.empty

smallestArtifact :: Ord a => (IdeArtifact -> a) -> [IdeArtifact] -> Maybe IdeArtifact
smallestArtifact tieBreaker = minimumByMay (compare `on` (\a -> (artifactSize a, tieBreaker a)))

artifactSize :: IdeArtifact -> (Int, Int)
artifactSize (IdeArtifact {..}) =
  ( P.sourcePosLine (P.spanEnd iaSpan) - P.sourcePosLine (P.spanStart iaSpan),
    P.sourcePosColumn (P.spanEnd iaSpan) - P.sourcePosColumn (P.spanStart iaSpan)
  )



getArtifactsAtPosition :: P.SourcePos -> IdeArtifacts -> [IdeArtifact]
getArtifactsAtPosition pos (IdeArtifacts m _) =
  Map.lookup (P.sourcePosLine pos) m
    & fromMaybe []
    & filter (\ia -> P.sourcePosColumn (P.spanStart (iaSpan ia)) <= posCol && P.sourcePosColumn (P.spanEnd (iaSpan ia)) >= posCol)
  where
    posCol = P.sourcePosColumn pos

insertIaExpr :: Text -> P.Expr -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaExpr label expr ty = case ss of
  Just span | not (generatedExpr expr) -> insertAtLines span (IaExpr label expr) ty mName defSpan
  _ -> identity
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
insertAtLines span value ty mName defSpan (IdeArtifacts m u) = IdeArtifacts m (foldr insert u (linesFromSpan span))
  where
    insert line = Map.insertWith (<>) line [IdeArtifact span value ty mName defSpan]
    
linesFromSpan :: P.SourceSpan -> [Line]
linesFromSpan ss = [P.sourcePosLine $ P.spanStart ss .. P.sourcePosLine $ P.spanEnd ss]

generatedExpr :: P.Expr -> Bool
generatedExpr = \case
  P.Var _ ident -> generatedIdent $ P.disqualify ident
  P.Constructor _ q -> generatedName $ P.disqualify q
  P.Abs b e -> generatedBinder b || generatedExpr e
  P.App e e' -> generatedExpr e || generatedExpr e'
  P.TypedValue _ e _ -> generatedExpr e
  P.PositionedValue _ _ e -> generatedExpr e
  P.Case es _ -> any generatedExpr es
  _ -> False

generatedName :: P.ProperName a -> Bool
generatedName = T.isSuffixOf "$Dict" . P.runProperName

generatedBinder :: P.Binder -> Bool
generatedBinder = \case
  P.VarBinder ss ident -> (ss == P.nullSourceSpan) || generatedIdent ident
  P.NamedBinder ss ident _ -> (ss == P.nullSourceSpan) || generatedIdent ident
  _ -> False

generatedIdent :: P.Ident -> Bool
generatedIdent = \case
  P.GenIdent {} -> True
  _ -> False

debugIdeArtifact :: IdeArtifact -> Text
debugIdeArtifact (IdeArtifact {..}) =
  show (P.sourcePosLine $ P.spanStart iaSpan)
    <> ":"
    <> show (P.sourcePosColumn $ P.spanStart iaSpan)
    <> "-"
    <> show (P.sourcePosLine $ P.spanEnd iaSpan)
    <> ":"
    <> show (P.sourcePosColumn $ P.spanEnd iaSpan)
    <> "\n"
    <> "Value: "
    <> debugIdeArtifactValue iaValue
    <> "\n"
    <> "Type: "
    <> debugType iaType

debugIdeArtifactValue :: IdeArtifactValue -> Text
debugIdeArtifactValue = \case
  IaExpr label expr -> "Expr: " <> label <> "\n" <> T.pack (take 64 $ render $ P.prettyPrintValue 5 expr)
  IaDecl d -> "Decl: " <> maybe "_" printName (P.declName d)
  IaBinder binder -> "Binder: " <> show binder
  IaIdent ident -> "Ident: " <> ident
  IaType t -> "Type " <> debugType t
  IaTypeName name -> "TypeName: " <> P.runProperName name
  IaClassName name -> "ClassName: " <> P.runProperName name

debugType :: P.Type a -> Text
debugType = T.pack . take 64 . P.prettyPrintType 5