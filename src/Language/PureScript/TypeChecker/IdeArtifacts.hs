
-- | Stores information about the source code that is useful for the IDE
-- | This includes value types and source spans
module Language.PureScript.TypeChecker.IdeArtifacts
  ( IdeArtifacts,
    IdeArtifact (..),
    IdeArtifactValue (..),
    artifactsAtSpan,
    getArtifactsAtPosition,
    emptyIdeArtifacts,
    insertIaExpr,
    insertIaBinder,
    insertIaDecl,
    insertIaType,
    insertIaIdent,
    insertTypeSynonym,
    insertModule,
    insertImport,
    useSynonymns,
    debugSynonyms,
    smallestArtifact,
    debugIdeArtifacts,
    insertIaTypeName,
    insertIaClassName,
    moduleNameFromQual,
    debugIdeArtifact,
    substituteArtifactTypes,
    endSubstitutions,
    artifactInterest,
  )
where

-- import Language.PureScript qualified as P

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.PureScript.AST.Binders qualified as P
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.AST.SourcePos qualified as P
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Names qualified as P
import Language.PureScript.Pretty.Types qualified as P
import Language.PureScript.Types qualified as P
import Protolude
import Safe (minimumByMay)

data IdeArtifacts
  = IdeArtifacts
      (Map Line (Set IdeArtifact)) -- with type var substitutions
      (Map Line (Set IdeArtifact)) -- without var substitutions
      (Map (P.Type ()) (P.Type ())) -- type synonym substitutions
  deriving (Show)

type Line = Int

emptyIdeArtifacts :: IdeArtifacts
emptyIdeArtifacts = IdeArtifacts Map.empty Map.empty Map.empty

debugIdeArtifacts :: IdeArtifacts -> Text
debugIdeArtifacts = T.intercalate "\n" . fmap showCount . lineCounts
  where
    showCount :: (Int, Int) -> Text
    showCount (line, count) = show line <> ": " <> show count
    lineCounts :: IdeArtifacts -> [(Int, Int)]
    lineCounts (IdeArtifacts m _ _) = Map.toList m <&> fmap length

data IdeArtifact = IdeArtifact
  { iaSpan :: P.SourceSpan,
    iaValue :: IdeArtifactValue,
    iaType :: P.SourceType,
    iaDefinitionModule :: Maybe P.ModuleName,
    iaDefinitionPos :: Maybe (Either P.SourcePos P.SourceSpan)
  }
  deriving (Show, Eq, Ord)

data IdeArtifactValue
  = IaExpr Text (Maybe Text) (Maybe LspNameType)
  | IaDecl (Maybe Text) (Maybe LspNameType)
  | IaBinder P.Binder
  | IaIdent Text
  | IaType P.SourceType
  | IaTypeName (P.ProperName 'P.TypeName)
  | IaClassName (P.ProperName 'P.ClassName)
  | IaModule P.ModuleName
  | IaImport P.ModuleName P.DeclarationRef
  deriving (Show, Ord, Eq)

substituteArtifactTypes :: (P.SourceType -> P.SourceType) -> IdeArtifacts -> IdeArtifacts
substituteArtifactTypes f (IdeArtifacts m u s) = IdeArtifacts m (Map.map (Set.map (onArtifactType f)) u) s

onArtifactType :: (P.SourceType -> P.SourceType) -> IdeArtifact -> IdeArtifact
onArtifactType f (IdeArtifact {..}) = IdeArtifact iaSpan iaValue (f iaType) iaDefinitionModule iaDefinitionPos

endSubstitutions :: IdeArtifacts -> IdeArtifacts
endSubstitutions (IdeArtifacts m u s) = IdeArtifacts (Map.unionWith (<>) m u) Map.empty s

smallestArtifact :: (Ord a) => (IdeArtifact -> a) -> [IdeArtifact] -> Maybe IdeArtifact
smallestArtifact tieBreaker = minimumByMay (compare `on` (\a -> (artifactSize a, tieBreaker a)))

-- | Prioritize artifacts that are more likely to be interesting to the developer on hover or click
artifactInterest :: IdeArtifact -> Int
artifactInterest (IdeArtifact {..}) = case iaValue of
  IaBinder {} -> 2
  IaTypeName {} -> 3
  IaClassName {} -> 3
  _ -> 1

artifactsAtSpan :: P.SourceSpan -> IdeArtifacts -> Set IdeArtifact
artifactsAtSpan span (IdeArtifacts m _ _) =
  Map.lookup (P.sourcePosLine $ P.spanStart span) m
    & maybe Set.empty (Set.filter ((==) span . iaSpan))

artifactSize :: IdeArtifact -> (Int, Int)
artifactSize (IdeArtifact {..}) =
  ( P.sourcePosLine (P.spanEnd iaSpan) - P.sourcePosLine (P.spanStart iaSpan),
    P.sourcePosColumn (P.spanEnd iaSpan) - P.sourcePosColumn (P.spanStart iaSpan)
  )

getArtifactsAtPosition :: P.SourcePos -> IdeArtifacts -> [IdeArtifact]
getArtifactsAtPosition pos (IdeArtifacts m _ _) =
  Map.lookup (P.sourcePosLine pos) m
    & maybe [] Set.toList
    & filter (\ia -> P.sourcePosColumn (P.spanStart (iaSpan ia)) <= posCol && P.sourcePosColumn (P.spanEnd (iaSpan ia)) >= posCol)
  where
    posCol = P.sourcePosColumn pos

insertIaExpr :: P.Expr -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaExpr expr ty = case ss of
  Just span
    | not (generatedExpr expr) ->
        insertAtLines span (IaExpr (exprCtr expr <> ": " <> fromMaybe "_" exprIdent) exprIdent (exprNameType expr)) ty mName defSpan
    where
      defSpan =
        Left <$> (posFromQual =<< exprIdentQual expr)

      mName = exprIdentQual expr >>= moduleNameFromQual

      exprIdent :: Maybe Text
      exprIdent = P.disqualify <$> exprIdentQual expr

      exprIdentQual :: P.Expr -> Maybe (P.Qualified Text)
      exprIdentQual = \case
        P.Var _ ident -> Just $ P.runIdent <$> ident
        P.Constructor _ q -> Just $ P.runProperName <$> q
        P.Op _ q -> Just $ P.runOpName <$> q
        P.PositionedValue _ _ e -> exprIdentQual e
        P.TypedValue _ e _ -> exprIdentQual e
        P.App e (P.TypeClassDictionary {}) -> exprIdentQual e
        _ -> Nothing

      exprNameType :: P.Expr -> Maybe LspNameType
      exprNameType = \case
        P.Var _ _ -> Just IdentNameType
        P.Constructor _ _ -> Just DctorNameType
        P.Op _ _ -> Just ValOpNameType
        P.PositionedValue _ _ e -> exprNameType e
        P.TypedValue _ e _ -> exprNameType e
        P.App e (P.TypeClassDictionary {}) -> exprNameType e
        _ -> Nothing
  _ -> identity
  where
    ss = P.exprSourceSpan expr

ellipsis :: Int -> Text -> Text
ellipsis n t = if T.length t > n then T.take (n - 3) t <> "..." else t

insertIaIdent :: P.SourceSpan -> P.Ident -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaIdent ss ident ty = case ident of
  P.Ident ident' -> insertAtLines ss (IaIdent ident') ty Nothing (Just $ Right ss)
  _ -> identity

insertIaBinder :: P.Binder -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaBinder binder ty = case binderSourceSpan binder of
  Just ss -> insertAtLines ss (IaBinder binder) ty Nothing (Just $ Right ss)
  Nothing -> identity

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

insertIaDecl :: P.Declaration -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaDecl decl ty = insertAtLines (P.declSourceSpan decl) (IaDecl (printDecl decl) (declNameType decl)) ty Nothing Nothing

printDecl :: P.Declaration -> Maybe Text
printDecl = fmap printName . P.declName

declNameType :: P.Declaration -> Maybe LspNameType
declNameType = \case
  P.DataDeclaration {} -> Just TyNameType
  P.TypeSynonymDeclaration {} -> Just TyNameType
  P.TypeClassDeclaration {} -> Just TyClassNameType
  P.TypeInstanceDeclaration {} -> Just IdentNameType
  P.KindDeclaration {} -> Just KindNameType
  P.ValueDeclaration {} -> Just IdentNameType
  _ -> Nothing

insertIaType :: P.SourceType -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaType ty kind = insertAtLines (fst $ P.getAnnForType ty) (IaType ty) kind Nothing Nothing

insertIaTypeName :: P.SourceSpan -> P.ProperName 'P.TypeName -> Maybe P.ModuleName -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaTypeName ss name mName kind = insertAtLines ss (IaTypeName name) kind mName (Just $ Right $ fst $ P.getAnnForType kind)

insertIaClassName :: P.SourceSpan -> P.ProperName 'P.ClassName -> Maybe P.ModuleName -> P.SourceType -> IdeArtifacts -> IdeArtifacts
insertIaClassName ss name mName kind = insertAtLines ss (IaClassName name) kind mName (Just $ Right $ fst $ P.getAnnForType kind)

insertModule :: P.SourceSpan -> P.ModuleName -> IdeArtifacts -> IdeArtifacts
insertModule ss name = insertAtLines ss (IaModule name) P.srcREmpty (Just name) Nothing

insertImport :: P.ModuleName -> P.DeclarationRef -> IdeArtifacts -> IdeArtifacts
insertImport name ref = insertAtLines (P.declRefSourceSpan ref) (IaImport name ref) P.srcREmpty (Just name) Nothing

posFromQual :: P.Qualified a -> Maybe P.SourcePos
posFromQual (P.Qualified (P.BySourcePos pos) _) = Just pos
posFromQual _ = Nothing

moduleNameFromQual :: P.Qualified a -> Maybe P.ModuleName
moduleNameFromQual (P.Qualified (P.ByModuleName mn) _) = Just mn
moduleNameFromQual _ = Nothing

insertAtLines :: P.SourceSpan -> IdeArtifactValue -> P.SourceType -> Maybe P.ModuleName -> Maybe (Either P.SourcePos P.SourceSpan) -> IdeArtifacts -> IdeArtifacts
insertAtLines span@(P.SourceSpan _ start _) value ty mName defSpan ia@(IdeArtifacts m u s) =
  if start == P.SourcePos 0 0 || start == P.SourcePos 1 1 -- ignore internal module spans
    then ia
    else IdeArtifacts m (foldr insert u (linesFromSpan span)) s
  where
    insert line = Map.insertWith Set.union line (Set.singleton $ IdeArtifact span value ty mName defSpan)

linesFromSpan :: P.SourceSpan -> [Line]
linesFromSpan ss = [P.sourcePosLine $ P.spanStart ss .. P.sourcePosLine $ P.spanEnd ss]

generatedExpr :: P.Expr -> Bool
generatedExpr = \case
  P.Var _ ident -> generatedIdent $ P.disqualify ident
  P.Constructor _ q -> generatedName $ P.disqualify q
  P.Abs b _e -> generatedBinder b
  P.TypedValue _ e _ -> generatedExpr e
  P.PositionedValue _ _ e -> generatedExpr e
  P.Unused {} -> True
  P.DeferredDictionary {} -> True
  P.TypeClassDictionary {} -> True
  P.DerivedInstancePlaceholder {} -> True
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

insertTypeSynonym :: P.Type a -> P.Type a -> IdeArtifacts -> IdeArtifacts
insertTypeSynonym syn ty (IdeArtifacts m u s) = IdeArtifacts m u (Map.insert (void syn) (void ty) s)

useSynonymns :: forall a. IdeArtifacts -> P.Type a -> P.Type ()
useSynonymns (IdeArtifacts _ _ s) ty = P.everywhereOnTypes go (void ty)
  where
    go :: P.Type () -> P.Type ()
    go t =
      Map.lookup t s
        & maybe t go

debugSynonyms :: IdeArtifacts -> Text
debugSynonyms (IdeArtifacts _ _ s) =
  show $
    Map.toList s
      <&> bimap
        (ellipsis 100 . T.pack . P.prettyPrintType 3)
        (ellipsis 100 . T.pack . P.prettyPrintType 3)

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
  IaExpr t _ _ -> "Expr: " <> t
  IaDecl d _ -> "Decl: " <> fromMaybe "_" d
  IaBinder binder -> "Binder: " <> show binder
  IaIdent ident -> "Ident: " <> ident
  IaType t -> "Type " <> debugType t
  IaTypeName name -> "TypeName: " <> P.runProperName name
  IaClassName name -> "ClassName: " <> P.runProperName name
  IaModule name -> "Module: " <> P.runModuleName name
  IaImport name ref -> "Import: " <> P.runModuleName name <> "." <> show ref

debugType :: P.Type a -> Text
debugType = T.pack . take 64 . P.prettyPrintType 5

exprCtr :: P.Expr -> Text
exprCtr (P.Literal _ _) = "Literal"
exprCtr (P.UnaryMinus _ _) = "UnaryMinus"
exprCtr (P.BinaryNoParens _ _ _) = "BinaryNoParens"
exprCtr (P.Parens _) = "Parens"
exprCtr (P.Accessor _ _) = "Accessor"
exprCtr (P.ObjectUpdate _ _) = "ObjectUpdate"
exprCtr (P.ObjectUpdateNested _ _) = "ObjectUpdateNested"
exprCtr (P.Abs _ _) = "Abs"
exprCtr (P.App e e') = "App (" <> exprCtr e <> ") (" <> exprCtr e' <> ")"
exprCtr (P.VisibleTypeApp _ _) = "VisibleTypeApp"
exprCtr (P.Unused e) = "Unused " <> exprCtr e
exprCtr (P.Var _ _) = "Var"
exprCtr (P.Op _ _) = "Op"
exprCtr (P.IfThenElse _ _ _) = "IfThenElse"
exprCtr (P.Constructor _ _) = "Constructor"
exprCtr (P.Case _ _) = "Case"
exprCtr (P.TypedValue _ e _) = "TypedValue " <> exprCtr e
exprCtr (P.Let _ _ _) = "Let"
exprCtr (P.Do _ _) = "Do"
exprCtr (P.Ado _ _ _) = "Ado"
exprCtr (P.TypeClassDictionary _ _ _) = "TypeClassDictionary"
exprCtr (P.DeferredDictionary _ _) = "DeferredDictionary"
exprCtr (P.DerivedInstancePlaceholder _ _) = "DerivedInstancePlaceholder"
exprCtr P.AnonymousArgument = "AnonymousArgument"
exprCtr (P.Hole _) = "Hole"
exprCtr (P.PositionedValue _ _ e) = "PositionedValue " <> exprCtr e
