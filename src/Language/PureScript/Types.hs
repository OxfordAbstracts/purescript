-- |
-- Data types for types
--
module Language.PureScript.Types where

import Prelude
import Protolude (ordNub, fromMaybe)

import Codec.Serialise (Serialise)
import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.DeepSeq (NFData)
import Control.Lens (Lens', (^.), set)
import Control.Monad ((<=<), (>=>))
import Data.Aeson ((.:), (.:?), (.!=), (.=))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Foldable (fold, foldl')
import Data.IntSet qualified as IS
import Data.List (sortOn)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Language.PureScript.AST.SourcePos (pattern NullSourceAnn, SourceAnn, SourceSpan)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Names (OpName, OpNameType(..), ProperName, ProperNameType(..), Qualified, coerceProperName)
import Language.PureScript.Label (Label)
import Language.PureScript.PSString (PSString)

type SourceType = Type SourceAnn
type SourceConstraint = Constraint SourceAnn

-- |
-- An identifier for the scope of a skolem variable
--
newtype SkolemScope = SkolemScope { runSkolemScope :: Int }
  deriving (Show, Eq, Ord, A.ToJSON, A.FromJSON, Generic)

instance NFData SkolemScope
instance Serialise SkolemScope

-- |
-- Describes how a TypeWildcard should be presented to the user during
-- type checking: holes (?foo) are always emitted as errors, whereas unnamed
-- wildcards (_) default to warnings, but are ignored entirely if they are
-- contained by a binding with a complete (wildcard-free) type signature.
--
data WildcardData = HoleWildcard Text | UnnamedWildcard | IgnoredWildcard
  deriving (Show, Eq, Ord, Generic)

instance NFData WildcardData
instance Serialise WildcardData

data TypeVarVisibility
  = TypeVarVisible
  | TypeVarInvisible
  deriving (Show, Eq, Ord, Generic)

instance NFData TypeVarVisibility
instance Serialise TypeVarVisibility

typeVarVisibilityPrefix :: TypeVarVisibility -> Text
typeVarVisibilityPrefix = \case
  TypeVarVisible -> "@"
  TypeVarInvisible -> mempty

-- |
-- The type of types
--
data Type a
  -- | A unification variable of type Type
  = TUnknown a Int
  -- | A named type variable
  | TypeVar a Text
  -- | A type-level string
  | TypeLevelString a PSString
  -- | A type-level natural
  | TypeLevelInt a Integer
  -- | A type wildcard, as would appear in a partial type synonym
  | TypeWildcard a WildcardData
  -- | A type constructor
  | TypeConstructor a (Qualified (ProperName 'TypeName))
  -- | A type operator. This will be desugared into a type constructor during the
  -- "operators" phase of desugaring.
  | TypeOp a (Qualified (OpName 'TypeOpName))
  -- | A type application
  | TypeApp a (Type a) (Type a)
  -- | Explicit kind application
  | KindApp a (Type a) (Type a)
  -- | Forall quantifier
  | ForAll a TypeVarVisibility Text (Maybe (Type a)) (Type a) (Maybe SkolemScope)
  -- | A type with a set of type class constraints
  | ConstrainedType a (Constraint a) (Type a)
  -- | A skolem constant
  | Skolem a Text (Maybe (Type a)) Int SkolemScope
  -- | An empty row
  | REmpty a
  -- | A non-empty row
  | RCons a Label (Type a) (Type a)
  -- | A type with a kind annotation
  | KindedType a (Type a) (Type a)
  -- | Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  | BinaryNoParensType a (Type a) (Type a) (Type a)
  -- | Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  | ParensInType a (Type a)
  deriving (Show, Generic, Functor, Foldable, Traversable)

instance NFData a => NFData (Type a)
instance Serialise a => Serialise (Type a)

srcTUnknown :: Int -> SourceType
srcTUnknown = TUnknown NullSourceAnn

srcTypeVar :: Text -> SourceType
srcTypeVar = TypeVar NullSourceAnn

srcTypeLevelString :: PSString -> SourceType
srcTypeLevelString = TypeLevelString NullSourceAnn

srcTypeLevelInt :: Integer -> SourceType
srcTypeLevelInt = TypeLevelInt NullSourceAnn

srcTypeWildcard :: SourceType
srcTypeWildcard = TypeWildcard NullSourceAnn UnnamedWildcard

srcTypeConstructor :: Qualified (ProperName 'TypeName) -> SourceType
srcTypeConstructor = TypeConstructor NullSourceAnn

srcTypeApp :: SourceType -> SourceType -> SourceType
srcTypeApp = TypeApp NullSourceAnn

srcKindApp :: SourceType -> SourceType -> SourceType
srcKindApp = KindApp NullSourceAnn

srcForAll :: TypeVarVisibility -> Text -> Maybe SourceType -> SourceType -> Maybe SkolemScope -> SourceType
srcForAll = ForAll NullSourceAnn

srcConstrainedType :: SourceConstraint -> SourceType -> SourceType
srcConstrainedType = ConstrainedType NullSourceAnn

srcREmpty :: SourceType
srcREmpty = REmpty NullSourceAnn

srcRCons :: Label -> SourceType -> SourceType -> SourceType
srcRCons = RCons NullSourceAnn

srcKindedType :: SourceType -> SourceType -> SourceType
srcKindedType = KindedType NullSourceAnn

pattern REmptyKinded :: forall a. a -> Maybe (Type a) -> Type a
pattern REmptyKinded ann mbK <- (toREmptyKinded -> Just (ann, mbK))

toREmptyKinded :: forall a. Type a -> Maybe (a, Maybe (Type a))
toREmptyKinded (REmpty ann) = Just (ann, Nothing)
toREmptyKinded (KindApp _ (REmpty ann) k) = Just (ann, Just k)
toREmptyKinded _ = Nothing

isREmpty :: forall a. Type a -> Bool
isREmpty = isJust . toREmptyKinded

-- | Additional data relevant to type class constraints
data ConstraintData
  = PartialConstraintData [[Text]] Bool
  -- ^ Data to accompany a Partial constraint generated by the exhaustivity checker.
  -- It contains (rendered) binder information for those binders which were
  -- not matched, and a flag indicating whether the list was truncated or not.
  -- Note: we use 'Text' here because using 'Binder' would introduce a cyclic
  -- dependency in the module graph.
  deriving (Show, Eq, Ord, Generic)

instance NFData ConstraintData
instance Serialise ConstraintData

-- | A typeclass constraint
data Constraint a = Constraint
  { constraintAnn :: a
  -- ^ constraint annotation
  , constraintClass :: Qualified (ProperName 'ClassName)
  -- ^ constraint class name
  , constraintKindArgs :: [Type a]
  -- ^ kind arguments
  , constraintArgs  :: [Type a]
  -- ^ type arguments
  , constraintData  :: Maybe ConstraintData
  -- ^ additional data relevant to this constraint
  } deriving (Show, Generic, Functor, Foldable, Traversable)

instance NFData a => NFData (Constraint a)
instance Serialise a => Serialise (Constraint a)

srcConstraint :: Qualified (ProperName 'ClassName) -> [SourceType] -> [SourceType] -> Maybe ConstraintData -> SourceConstraint
srcConstraint = Constraint NullSourceAnn

mapConstraintArgs :: ([Type a] -> [Type a]) -> Constraint a -> Constraint a
mapConstraintArgs f c = c { constraintArgs = f (constraintArgs c) }

overConstraintArgs :: Functor f => ([Type a] -> f [Type a]) -> Constraint a -> f (Constraint a)
overConstraintArgs f c = (\args -> c { constraintArgs = args }) <$> f (constraintArgs c)

mapConstraintArgsAll :: ([Type a] -> [Type a]) -> Constraint a -> Constraint a
mapConstraintArgsAll f c =
  c { constraintKindArgs = f (constraintKindArgs c)
    , constraintArgs = f (constraintArgs c)
    }

overConstraintArgsAll :: Applicative f => ([Type a] -> f [Type a]) -> Constraint a -> f (Constraint a)
overConstraintArgsAll f c =
  (\a b -> c { constraintKindArgs = a, constraintArgs = b })
    <$> f (constraintKindArgs c)
    <*> f (constraintArgs c)

constraintDataToJSON :: ConstraintData -> A.Value
constraintDataToJSON (PartialConstraintData bs trunc) =
  A.object
    [ "contents" .= (bs, trunc)
    ]

constraintToJSON :: (a -> A.Value) -> Constraint a -> A.Value
constraintToJSON annToJSON Constraint {..} =
  A.object
    [ "constraintAnn"   .= annToJSON constraintAnn
    , "constraintClass" .= constraintClass
    , "constraintKindArgs"  .= fmap (typeToJSON annToJSON) constraintKindArgs
    , "constraintArgs"  .= fmap (typeToJSON annToJSON) constraintArgs
    , "constraintData"  .= fmap constraintDataToJSON constraintData
    ]

typeVarVisToJSON :: TypeVarVisibility -> A.Value
typeVarVisToJSON = \case
  TypeVarVisible -> A.toJSON ("TypeVarVisible" :: Text)
  TypeVarInvisible -> A.toJSON ("TypeVarInvisible" :: Text)

typeToJSON :: forall a. (a -> A.Value) -> Type a -> A.Value
typeToJSON annToJSON ty =
  case ty of
    TUnknown a b ->
      variant "TUnknown" a b
    TypeVar a b ->
      variant "TypeVar" a b
    TypeLevelString a b ->
      variant "TypeLevelString" a b
    TypeLevelInt a b ->
      variant "TypeLevelInt" a b
    TypeWildcard a b ->
      variant "TypeWildcard" a b
    TypeConstructor a b ->
      variant "TypeConstructor" a b
    TypeOp a b ->
      variant "TypeOp" a b
    TypeApp a b c ->
      variant "TypeApp" a (go b, go c)
    KindApp a b c ->
      variant "KindApp" a (go b, go c)
    ForAll a b c d e f ->
      variant "ForAll" a $ A.object
        [ "visibility" .= b
        , "identifier" .= c
        , "kind" .= fmap go d
        , "type" .= go e
        , "skolem" .= f
        ]
    ConstrainedType a b c ->
      variant "ConstrainedType" a (constraintToJSON annToJSON b, go c)
    Skolem a b c d e ->
      variant "Skolem" a (b, go <$> c, d, e)
    REmpty a ->
      nullary "REmpty" a
    RCons a b c d ->
      variant "RCons" a (b, go c, go d)
    KindedType a b c ->
      variant "KindedType" a (go b, go c)
    BinaryNoParensType a b c d ->
      variant "BinaryNoParensType" a (go b, go c, go d)
    ParensInType a b ->
      variant "ParensInType" a (go b)
  where
  go :: Type a -> A.Value
  go = typeToJSON annToJSON

  variant :: A.ToJSON b => String -> a -> b -> A.Value
  variant tag ann contents =
    A.object
      [ "tag"        .= tag
      , "annotation" .= annToJSON ann
      , "contents"   .= contents
      ]

  nullary :: String -> a -> A.Value
  nullary tag ann =
    A.object
      [ "tag"        .= tag
      , "annotation" .= annToJSON ann
      ]

instance A.ToJSON WildcardData where
  toJSON = \case
    HoleWildcard name -> A.String name
    UnnamedWildcard -> A.Null
    IgnoredWildcard -> A.object [ "ignored" .= True ]

instance A.ToJSON a => A.ToJSON (Type a) where
  toJSON = typeToJSON A.toJSON

instance A.ToJSON a => A.ToJSON (Constraint a) where
  toJSON = constraintToJSON A.toJSON

instance A.ToJSON ConstraintData where
  toJSON = constraintDataToJSON

instance A.ToJSON TypeVarVisibility where
  toJSON = typeVarVisToJSON

constraintDataFromJSON :: A.Value -> A.Parser ConstraintData
constraintDataFromJSON = A.withObject "PartialConstraintData" $ \o -> do
  (bs, trunc) <- o .: "contents"
  pure $ PartialConstraintData bs trunc

constraintFromJSON :: forall a. A.Parser a -> (A.Value -> A.Parser a) -> A.Value -> A.Parser (Constraint a)
constraintFromJSON defaultAnn annFromJSON = A.withObject "Constraint" $ \o -> do
  constraintAnn   <- (o .: "constraintAnn" >>= annFromJSON) <|> defaultAnn
  constraintClass <- o .: "constraintClass"
  constraintKindArgs <- o .:? "constraintKindArgs" .!= [] >>= traverse (typeFromJSON defaultAnn annFromJSON)
  constraintArgs  <- o .: "constraintArgs" >>= traverse (typeFromJSON defaultAnn annFromJSON)
  constraintData  <- o .: "constraintData" >>= traverse constraintDataFromJSON
  pure $ Constraint {..}

typeVarVisFromJSON :: A.Value -> A.Parser TypeVarVisibility
typeVarVisFromJSON v = do
  v' <- A.parseJSON v
  case v' of
    "TypeVarVisible" -> pure TypeVarVisible
    "TypeVarInvisible" -> pure TypeVarInvisible
    _ -> fail $ "Unrecognized TypeVarVisibility: " <> v'

typeFromJSON :: forall a. A.Parser a -> (A.Value -> A.Parser a) -> A.Value -> A.Parser (Type a)
typeFromJSON defaultAnn annFromJSON = A.withObject "Type" $ \o -> do
  tag <- o .: "tag"
  a   <- (o .: "annotation" >>= annFromJSON) <|> defaultAnn
  let
    contents :: A.FromJSON b => A.Parser b
    contents = o .: "contents"
  case tag of
    "TUnknown" ->
      TUnknown a <$> contents
    "TypeVar" ->
      TypeVar a <$> contents
    "TypeLevelString" ->
      TypeLevelString a <$> contents
    "TypeLevelInt" ->
      TypeLevelInt a <$> contents
    "TypeWildcard" -> do
      b <- contents <|> pure UnnamedWildcard
      pure $ TypeWildcard a b
    "TypeConstructor" ->
      TypeConstructor a <$> contents
    "TypeOp" ->
      TypeOp a <$> contents
    "TypeApp" -> do
      (b, c) <- contents
      TypeApp a <$> go b <*> go c
    "KindApp" -> do
      (b, c) <- contents
      KindApp a <$> go b <*> go c
    "ForAll" -> do
      let
        asObject = do
          f <- contents
          v <- f .: "visibility"
          i <- f .: "identifier"
          k <- f .:? "kind"
          t <- f .: "type"
          s <- f .: "skolem"
          ForAll a v i <$> traverse go k <*> go t <*> pure s

        withoutMbKind = do
          (b, c, d) <- contents
          ForAll a TypeVarInvisible b Nothing <$> go c <*> pure d

        withMbKind = do
          (b, c, d, e) <- contents
          ForAll a TypeVarInvisible b <$> (Just <$> go c) <*> go d <*> pure e
      asObject <|> withMbKind <|> withoutMbKind
    "ConstrainedType" -> do
      (b, c) <- contents
      ConstrainedType a <$> constraintFromJSON defaultAnn annFromJSON b <*> go c
    "Skolem" -> do
      (b, c, d, e) <- contents
      c' <- traverse go c
      pure $ Skolem a b c' d e
    "REmpty" ->
      pure $ REmpty a
    "RCons" -> do
      (b, c, d) <- contents
      RCons a b <$> go c <*> go d
    "KindedType" -> do
      (b, c) <- contents
      KindedType a <$> go b <*> go c
    "BinaryNoParensType" -> do
      (b, c, d) <- contents
      BinaryNoParensType a <$> go b <*> go c <*> go d
    "ParensInType" -> do
      b <- contents
      ParensInType a <$> go b
    -- Backwards compatibility for kinds
    "KUnknown" ->
      TUnknown a <$> contents
    "Row" ->
      TypeApp a (TypeConstructor a C.Row) <$> (go =<< contents)
    "FunKind" -> do
      (b, c) <- contents
      TypeApp a . TypeApp a (TypeConstructor a C.Function) <$> go b <*> go c
    "NamedKind" ->
      TypeConstructor a <$> contents
    other ->
      fail $ "Unrecognised tag: " ++ other
  where
  go :: A.Value -> A.Parser (Type a)
  go = typeFromJSON defaultAnn annFromJSON

-- These overlapping instances exist to preserve compatibility for common
-- instances which have a sensible default for missing annotations.
instance {-# OVERLAPPING #-} A.FromJSON (Type SourceAnn) where
  parseJSON = typeFromJSON (pure NullSourceAnn) A.parseJSON

instance {-# OVERLAPPING #-} A.FromJSON (Type ()) where
  parseJSON = typeFromJSON (pure ()) A.parseJSON

instance {-# OVERLAPPING #-} A.FromJSON a => A.FromJSON (Type a) where
  parseJSON = typeFromJSON (fail "Invalid annotation") A.parseJSON

instance {-# OVERLAPPING #-} A.FromJSON (Constraint SourceAnn) where
  parseJSON = constraintFromJSON (pure NullSourceAnn) A.parseJSON

instance {-# OVERLAPPING #-} A.FromJSON (Constraint ()) where
  parseJSON = constraintFromJSON (pure ()) A.parseJSON

instance {-# OVERLAPPING #-} A.FromJSON a => A.FromJSON (Constraint a) where
  parseJSON = constraintFromJSON (fail "Invalid annotation") A.parseJSON

instance A.FromJSON ConstraintData where
  parseJSON = constraintDataFromJSON

instance A.FromJSON WildcardData where
  parseJSON = \case
    A.String name -> pure $ HoleWildcard name
    A.Object _ -> pure IgnoredWildcard
    A.Null -> pure UnnamedWildcard
    _ -> fail "Unrecognized WildcardData"

instance A.FromJSON TypeVarVisibility where
  parseJSON = typeVarVisFromJSON

data RowListItem a = RowListItem
  { rowListAnn :: a
  , rowListLabel :: Label
  , rowListType :: Type a
  } deriving (Show, Generic, Functor, Foldable, Traversable)

srcRowListItem :: Label -> SourceType -> RowListItem SourceAnn
srcRowListItem = RowListItem NullSourceAnn

-- | Convert a row to a list of pairs of labels and types
rowToList :: Type a -> ([RowListItem a], Type a)
rowToList = go where
  go (RCons ann name ty row) =
    first (RowListItem ann name ty :) (rowToList row)
  go r = ([], r)

-- | Convert a row to a list of pairs of labels and types, sorted by the labels.
rowToSortedList :: Type a -> ([RowListItem a], Type a)
rowToSortedList = first (sortOn rowListLabel) . rowToList

-- | Convert a list of labels and types to a row
rowFromList :: ([RowListItem a], Type a) -> Type a
rowFromList (xs, r) = foldr (\(RowListItem ann name ty) -> RCons ann name ty) r xs

-- | Align two rows of types, splitting them into three parts:
--
-- * Those types which appear in both rows
-- * Those which appear only on the left
-- * Those which appear only on the right
--
-- Note: importantly, we preserve the order of the types with a given label.
alignRowsWith
  :: (Label -> Type a -> Type a -> r)
  -> Type a
  -> Type a
  -> ([r], (([RowListItem a], Type a), ([RowListItem a], Type a)))
alignRowsWith f ty1 ty2 = go s1 s2 where
  (s1, tail1) = rowToSortedList ty1
  (s2, tail2) = rowToSortedList ty2

  go [] r = ([], (([], tail1), (r, tail2)))
  go r [] = ([], ((r, tail1), ([], tail2)))
  go lhs@(RowListItem a1 l1 t1 : r1) rhs@(RowListItem a2 l2 t2 : r2) = 
    case compare l1 l2 of
      LT -> (second . first . first) (RowListItem a1 l1 t1 :) (go r1 rhs)
      GT -> (second . second . first) (RowListItem a2 l2 t2 :) (go lhs r2)
      EQ -> first (f l1 t1 t2 :) (go r1 r2)

-- | Check whether a type is a monotype
isMonoType :: Type a -> Bool
isMonoType ForAll{} = False
isMonoType (ParensInType _ t) = isMonoType t
isMonoType (KindedType _ t _) = isMonoType t
isMonoType _        = True

-- | Universally quantify a type
mkForAll :: [(a, (Text, Maybe (Type a)))] -> Type a -> Type a
mkForAll args ty = foldr (\(ann, (arg, mbK)) t -> ForAll ann TypeVarInvisible arg mbK t Nothing) ty args

-- | Replace a type variable, taking into account variable shadowing
replaceTypeVars :: Text -> Type a -> Type a -> Type a
replaceTypeVars v r = replaceAllTypeVars [(v, r)]

-- | Replace named type variables with types
replaceAllTypeVars :: [(Text, Type a)] -> Type a -> Type a
replaceAllTypeVars = go [] where
  go :: [Text] -> [(Text, Type a)] -> Type a -> Type a
  go _  m (TypeVar ann v) = fromMaybe (TypeVar ann v) (v `lookup` m)
  go bs m (TypeApp ann t1 t2) = TypeApp ann (go bs m t1) (go bs m t2)
  go bs m (KindApp ann t1 t2) = KindApp ann (go bs m t1) (go bs m t2)
  go bs m (ForAll ann vis v mbK t sco)
    | v `elem` keys = go bs (filter ((/= v) . fst) m) $ ForAll ann vis v mbK' t sco
    | v `elem` usedVars =
      let v' = genPureName v (keys ++ bs ++ usedVars)
          t' = go bs [(v, TypeVar ann v')] t
      in ForAll ann vis v' mbK' (go (v' : bs) m t') sco
    | otherwise = ForAll ann vis v mbK' (go (v : bs) m t) sco
    where
      mbK' = go bs m <$> mbK
      keys = map fst m
      usedVars = concatMap (usedTypeVariables . snd) m
  go bs m (ConstrainedType ann c t) = ConstrainedType ann (mapConstraintArgsAll (map (go bs m)) c) (go bs m t)
  go bs m (RCons ann name' t r) = RCons ann name' (go bs m t) (go bs m r)
  go bs m (KindedType ann t k) = KindedType ann (go bs m t) (go bs m k)
  go bs m (BinaryNoParensType ann t1 t2 t3) = BinaryNoParensType ann (go bs m t1) (go bs m t2) (go bs m t3)
  go bs m (ParensInType ann t) = ParensInType ann (go bs m t)
  go _  _ ty = ty

genPureName :: Text -> [Text] -> Text
genPureName orig inUse = try' 0
  where
  try' :: Integer -> Text
  try' n | (orig <> T.pack (show n)) `elem` inUse = try' (n + 1)
         | otherwise = orig <> T.pack (show n)

-- | Add visible type abstractions to top-level foralls.
addVisibility :: [(Text, TypeVarVisibility)] -> Type a -> Type a
addVisibility v = go where
  go (ForAll ann vis arg mbK ty sco) = case lookup arg v of
    Just vis' ->
      ForAll ann vis' arg mbK (go ty) sco
    Nothing ->
      ForAll ann vis arg mbK (go ty) sco
  go (ParensInType ann ty) = ParensInType ann (go ty)
  go ty = ty

-- | Collect all type variables appearing in a type
usedTypeVariables :: Type a -> [Text]
usedTypeVariables = ordNub . everythingOnTypes (++) go where
  go (TypeVar _ v) = [v]
  go _ = []

-- | Collect all free type variables appearing in a type
freeTypeVariables :: Type a -> [Text]
freeTypeVariables = ordNub . fmap snd . sortOn fst . go 0 [] where
  -- Tracks kind levels so that variables appearing in kind annotations are listed first.
  go :: Int -> [Text] -> Type a -> [(Int, Text)]
  go lvl bound (TypeVar _ v) | v `notElem` bound = [(lvl, v)]
  go lvl bound (TypeApp _ t1 t2) = go lvl bound t1 ++ go lvl bound t2
  go lvl bound (KindApp _ t1 t2) = go lvl bound t1 ++ go (lvl - 1) bound t2
  go lvl bound (ForAll _ _ v mbK t _) = foldMap (go (lvl - 1) bound) mbK ++ go lvl (v : bound) t
  go lvl bound (ConstrainedType _ c t) = foldMap (go (lvl - 1) bound) (constraintKindArgs c) ++ foldMap (go lvl bound) (constraintArgs c) ++ go lvl bound t
  go lvl bound (RCons _ _ t r) = go lvl bound t ++ go lvl bound r
  go lvl bound (KindedType _ t k) = go lvl bound t ++ go (lvl - 1) bound k
  go lvl bound (BinaryNoParensType _ t1 t2 t3) = go lvl bound t1 ++ go lvl bound t2 ++ go lvl bound t3
  go lvl bound (ParensInType _ t) = go lvl bound t
  go _ _ _ = []

-- | Collect a complete set of kind-annotated quantifiers at the front of a type.
completeBinderList :: Type a -> Maybe ([(a, (Text, Type a))], Type a)
completeBinderList = go []
  where
  go acc = \case
    ForAll _ _ _ Nothing _ _ -> Nothing
    ForAll ann _ var (Just k) ty _ -> go ((ann, (var, k)) : acc) ty
    ty -> Just (reverse acc, ty)

-- | Universally quantify over all type variables appearing free in a type
quantify :: Type a -> Type a
quantify ty = foldr (\arg t -> ForAll (getAnnForType ty) TypeVarInvisible arg Nothing t Nothing) ty $ freeTypeVariables ty

-- | Move all universal quantifiers to the front of a type
moveQuantifiersToFront :: a -> Type a -> Type a
moveQuantifiersToFront syntheticAnn = go [] [] 
  where
  go qs cs = \case
    ForAll ann vis q mbK ty sco -> do
      let 
        cArgs :: [Text] = cs >>= constraintArgs . snd >>= freeTypeVariables
        (q'', ty')
          | q `elem` cArgs = do
              let q' = genPureName q $ cArgs <> freeTypeVariables ty
              (q', replaceTypeVars q (TypeVar syntheticAnn q') ty)
          | otherwise =
              (q, ty)
      go ((ann, q'', sco, mbK, vis) : qs) cs ty'
    ConstrainedType ann c ty ->
      go qs ((ann, c) : cs) ty
    ty -> 
      foldl (\ty' (ann, q, sco, mbK, vis) -> ForAll ann vis q mbK ty' sco) (foldl (\ty' (ann, c) -> ConstrainedType ann c ty') ty cs) qs

-- | Check if a type contains `forall`
containsForAll :: Type a -> Bool
containsForAll = everythingOnTypes (||) go where
  go :: Type a -> Bool
  go ForAll{} = True
  go _ = False

unknowns :: Type a -> IS.IntSet
unknowns = everythingOnTypes (<>) go where
  go :: Type a -> IS.IntSet
  go (TUnknown _ u) = IS.singleton u
  go _ = mempty

-- | Check if a type contains unknowns in a position that is relevant to
-- constraint solving. (Kinds are not.)
containsUnknowns :: Type a -> Bool
containsUnknowns = everythingOnTypes (||) go . eraseKindApps where
  go :: Type a -> Bool
  go TUnknown{} = True
  go _ = False

eraseKindApps :: Type a -> Type a
eraseKindApps = everywhereOnTypes $ \case
  KindApp _ ty _ -> ty
  ConstrainedType ann con ty ->
    ConstrainedType ann (con { constraintKindArgs = [] }) ty
  Skolem ann name _ i sc ->
    Skolem ann name Nothing i sc
  other -> other

eraseForAllKindAnnotations :: Type a -> Type a
eraseForAllKindAnnotations = removeAmbiguousVars . removeForAllKinds
  where
  removeForAllKinds = everywhereOnTypes $ \case
    ForAll ann vis arg _ ty sco ->
      ForAll ann vis arg Nothing ty sco
    other -> other

  removeAmbiguousVars = everywhereOnTypes $ \case
    fa@(ForAll _ _ arg _ ty _)
      | arg `elem` freeTypeVariables ty -> fa
      | otherwise -> ty
    other -> other

unapplyTypes :: Type a -> (Type a, [Type a], [Type a])
unapplyTypes = goTypes []
  where
  goTypes acc (TypeApp _ a b) = goTypes (b : acc) a
  goTypes acc a = let (ty, kinds) = goKinds [] a in (ty, kinds, acc)

  goKinds acc (KindApp _ a b) = goKinds (b : acc) a
  goKinds acc a = (a, acc)

unapplyConstraints :: Type a -> ([Constraint a], Type a)
unapplyConstraints = go []
  where
  go acc (ConstrainedType _ con ty) = go (con : acc) ty
  go acc ty = (reverse acc, ty)

-- | Construct the type of an instance declaration from its parts. Used in
-- error messages describing unnamed instances.
srcInstanceType
  :: SourceSpan
  -> [(Text, SourceType)]
  -> Qualified (ProperName 'ClassName)
  -> [SourceType]
  -> SourceType
srcInstanceType ss vars className tys
  = setAnnForType (ss, [])
  . flip (foldr $ \(tv, k) ty -> srcForAll TypeVarInvisible tv (Just k) ty Nothing) vars
  . flip (foldl' srcTypeApp) tys
  $ srcTypeConstructor $ coerceProperName <$> className

everywhereOnTypes :: (Type a -> Type a) -> Type a -> Type a
everywhereOnTypes f = go where
  go (TypeApp ann t1 t2) = f (TypeApp ann (go t1) (go t2))
  go (KindApp ann t1 t2) = f (KindApp ann (go t1) (go t2))
  go (ForAll ann vis arg mbK ty sco) = f (ForAll ann vis arg (go <$> mbK) (go ty) sco)
  go (ConstrainedType ann c ty) = f (ConstrainedType ann (mapConstraintArgsAll (map go) c) (go ty))
  go (Skolem ann name mbK i sc) = f (Skolem ann name (go <$> mbK) i sc)
  go (RCons ann name ty rest) = f (RCons ann name (go ty) (go rest))
  go (KindedType ann ty k) = f (KindedType ann (go ty) (go k))
  go (BinaryNoParensType ann t1 t2 t3) = f (BinaryNoParensType ann (go t1) (go t2) (go t3))
  go (ParensInType ann t) = f (ParensInType ann (go t))
  go other = f other

everywhereOnTypesM :: Monad m => (Type a -> m (Type a)) -> Type a -> m (Type a)
everywhereOnTypesM f = go where
  go (TypeApp ann t1 t2) = (TypeApp ann <$> go t1 <*> go t2) >>= f
  go (KindApp ann t1 t2) = (KindApp ann <$> go t1 <*> go t2) >>= f
  go (ForAll ann vis arg mbK ty sco) = (ForAll ann vis arg <$> traverse go mbK <*> go ty <*> pure sco) >>= f
  go (ConstrainedType ann c ty) = (ConstrainedType ann <$> overConstraintArgsAll (mapM go) c <*> go ty) >>= f
  go (Skolem ann name mbK i sc) = (Skolem ann name <$> traverse go mbK <*> pure i <*> pure sc) >>= f
  go (RCons ann name ty rest) = (RCons ann name <$> go ty <*> go rest) >>= f
  go (KindedType ann ty k) = (KindedType ann <$> go ty <*> go k) >>= f
  go (BinaryNoParensType ann t1 t2 t3) = (BinaryNoParensType ann <$> go t1 <*> go t2 <*> go t3) >>= f
  go (ParensInType ann t) = (ParensInType ann <$> go t) >>= f
  go other = f other
{-# INLINE everywhereOnTypesM #-}

everywhereOnTypesTopDownM :: Monad m => (Type a -> m (Type a)) -> Type a -> m (Type a)
everywhereOnTypesTopDownM f = go <=< f where
  go (TypeApp ann t1 t2) = TypeApp ann <$> (f t1 >>= go) <*> (f t2 >>= go)
  go (KindApp ann t1 t2) = KindApp ann <$> (f t1 >>= go) <*> (f t2 >>= go)
  go (ForAll ann vis arg mbK ty sco) = ForAll ann vis arg <$> traverse (f >=> go) mbK <*> (f ty >>= go) <*> pure sco
  go (ConstrainedType ann c ty) = ConstrainedType ann <$> overConstraintArgsAll (mapM (go <=< f)) c <*> (f ty >>= go)
  go (Skolem ann name mbK i sc) = Skolem ann name <$> traverse (f >=> go) mbK <*> pure i <*> pure sc
  go (RCons ann name ty rest) = RCons ann name <$> (f ty >>= go) <*> (f rest >>= go)
  go (KindedType ann ty k) = KindedType ann <$> (f ty >>= go) <*> (f k >>= go)
  go (BinaryNoParensType ann t1 t2 t3) = BinaryNoParensType ann <$> (f t1 >>= go) <*> (f t2 >>= go) <*> (f t3 >>= go)
  go (ParensInType ann t) = ParensInType ann <$> (f t >>= go)
  go other = pure other
{-# INLINE everywhereOnTypesTopDownM #-}

everythingOnTypes :: (r -> r -> r) -> (Type a -> r) -> Type a -> r
everythingOnTypes (<+>) f = go where
  go t@(TypeApp _ t1 t2) = f t <+> go t1 <+> go t2
  go t@(KindApp _ t1 t2) = f t <+> go t1 <+> go t2
  go t@(ForAll _ _ _ (Just k) ty _) = f t <+> go k <+> go ty
  go t@(ForAll _ _ _ _ ty _) = f t <+> go ty
  go t@(ConstrainedType _ c ty) = foldl (<+>) (f t) (map go (constraintKindArgs c) ++ map go (constraintArgs c)) <+> go ty
  go t@(Skolem _ _ (Just k) _ _) = f t <+> go k
  go t@(RCons _ _ ty rest) = f t <+> go ty <+> go rest
  go t@(KindedType _ ty k) = f t <+> go ty <+> go k
  go t@(BinaryNoParensType _ t1 t2 t3) = f t <+> go t1 <+> go t2 <+> go t3
  go t@(ParensInType _ t1) = f t <+> go t1
  go other = f other
{-# INLINE everythingOnTypes #-}

everythingWithContextOnTypes :: s -> r -> (r -> r -> r) -> (s -> Type a -> (s, r)) -> Type a -> r
everythingWithContextOnTypes s0 r0 (<+>) f = go' s0 where
  go' s t = let (s', r) = f s t in r <+> go s' t
  go s (TypeApp _ t1 t2) = go' s t1 <+> go' s t2
  go s (KindApp _ t1 t2) = go' s t1 <+> go' s t2
  go s (ForAll _ _ _ (Just k) ty _) = go' s k <+> go' s ty
  go s (ForAll _ _ _ _ ty _) = go' s ty
  go s (ConstrainedType _ c ty) = foldl (<+>) r0 (map (go' s) (constraintKindArgs c) ++ map (go' s) (constraintArgs c)) <+> go' s ty
  go s (Skolem _ _ (Just k) _ _) = go' s k
  go s (RCons _ _ ty rest) = go' s ty <+> go' s rest
  go s (KindedType _ ty k) = go' s ty <+> go' s k
  go s (BinaryNoParensType _ t1 t2 t3) = go' s t1 <+> go' s t2 <+> go' s t3
  go s (ParensInType _ t1) = go' s t1
  go _ _ = r0
{-# INLINE everythingWithContextOnTypes #-}

annForType :: Lens' (Type a) a
annForType k (TUnknown a b) = (\z -> TUnknown z b) <$> k a
annForType k (TypeVar a b) = (\z -> TypeVar z b) <$> k a
annForType k (TypeLevelString a b) = (\z -> TypeLevelString z b) <$> k a
annForType k (TypeLevelInt a b) = (\z -> TypeLevelInt z b) <$> k a
annForType k (TypeWildcard a b) = (\z -> TypeWildcard z b) <$> k a
annForType k (TypeConstructor a b) = (\z -> TypeConstructor z b) <$> k a
annForType k (TypeOp a b) = (\z -> TypeOp z b) <$> k a
annForType k (TypeApp a b c) = (\z -> TypeApp z b c) <$> k a
annForType k (KindApp a b c) = (\z -> KindApp z b c) <$> k a
annForType k (ForAll a b c d e f) = (\z -> ForAll z b c d e f) <$> k a
annForType k (ConstrainedType a b c) = (\z -> ConstrainedType z b c) <$> k a
annForType k (Skolem a b c d e) = (\z -> Skolem z b c d e) <$> k a
annForType k (REmpty a) = REmpty <$> k a
annForType k (RCons a b c d) = (\z -> RCons z b c d) <$> k a
annForType k (KindedType a b c) = (\z -> KindedType z b c) <$> k a
annForType k (BinaryNoParensType a b c d) = (\z -> BinaryNoParensType z b c d) <$> k a
annForType k (ParensInType a b) = (\z -> ParensInType z b) <$> k a

getAnnForType :: Type a -> a
getAnnForType = (^. annForType)

setAnnForType :: a -> Type a -> Type a
setAnnForType = set annForType

instance Eq (Type a) where
  (==) = eqType

instance Ord (Type a) where
  compare = compareType

eqType :: Type a -> Type b -> Bool
eqType (TUnknown _ a) (TUnknown _ a') = a == a'
eqType (TypeVar _ a) (TypeVar _ a') = a == a'
eqType (TypeLevelString _ a) (TypeLevelString _ a') = a == a'
eqType (TypeLevelInt _ a) (TypeLevelInt _ a') = a == a'
eqType (TypeWildcard _ a) (TypeWildcard _ a') = a == a'
eqType (TypeConstructor _ a) (TypeConstructor _ a') = a == a'
eqType (TypeOp _ a) (TypeOp _ a') = a == a'
eqType (TypeApp _ a b) (TypeApp _ a' b') = eqType a a' && eqType b b'
eqType (KindApp _ a b) (KindApp _ a' b') = eqType a a' && eqType b b'
eqType (ForAll _ _ a b c d) (ForAll _ _ a' b' c' d') = a == a' && eqMaybeType b b' && eqType c c' && d == d'
eqType (ConstrainedType _ a b) (ConstrainedType _ a' b') = eqConstraint a a' && eqType b b'
eqType (Skolem _ a b c d) (Skolem _ a' b' c' d') = a == a' && eqMaybeType b b' && c == c' && d == d'
eqType (REmpty _) (REmpty _) = True
eqType (RCons _ a b c) (RCons _ a' b' c') = a == a' && eqType b b' && eqType c c'
eqType (KindedType _ a b) (KindedType _ a' b') = eqType a a' && eqType b b'
eqType (BinaryNoParensType _ a b c) (BinaryNoParensType _ a' b' c') = eqType a a' && eqType b b' && eqType c c'
eqType (ParensInType _ a) (ParensInType _ a') = eqType a a'
eqType _ _ = False

eqMaybeType :: Maybe (Type a) -> Maybe (Type b) -> Bool
eqMaybeType (Just a) (Just b) = eqType a b
eqMaybeType Nothing Nothing = True
eqMaybeType _ _ = False

compareType :: Type a -> Type b -> Ordering
compareType (TUnknown _ a) (TUnknown _ a') = compare a a'
compareType (TypeVar _ a) (TypeVar _ a') = compare a a'
compareType (TypeLevelString _ a) (TypeLevelString _ a') = compare a a'
compareType (TypeLevelInt _ a) (TypeLevelInt _ a') = compare a a'
compareType (TypeWildcard _ a) (TypeWildcard _ a') = compare a a'
compareType (TypeConstructor _ a) (TypeConstructor _ a') = compare a a'
compareType (TypeOp _ a) (TypeOp _ a') = compare a a'
compareType (TypeApp _ a b) (TypeApp _ a' b') = compareType a a' <> compareType b b'
compareType (KindApp _ a b) (KindApp _ a' b') = compareType a a' <> compareType b b'
compareType (ForAll _ _ a b c d) (ForAll _ _ a' b' c' d') = compare a a' <> compareMaybeType b b' <> compareType c c' <> compare d d'
compareType (ConstrainedType _ a b) (ConstrainedType _ a' b') = compareConstraint a a' <> compareType b b'
compareType (Skolem _ a b c d) (Skolem _ a' b' c' d') = compare a a' <> compareMaybeType b b' <> compare c c' <> compare d d'
compareType (REmpty _) (REmpty _) = EQ
compareType (RCons _ a b c) (RCons _ a' b' c') = compare a a' <> compareType b b' <> compareType c c'
compareType (KindedType _ a b) (KindedType _ a' b') = compareType a a' <> compareType b b'
compareType (BinaryNoParensType _ a b c) (BinaryNoParensType _ a' b' c') = compareType a a' <> compareType b b' <> compareType c c'
compareType (ParensInType _ a) (ParensInType _ a') = compareType a a'
compareType typ typ' =
  compare (orderOf typ) (orderOf typ')
    where
      orderOf :: Type a -> Int
      orderOf TUnknown{} = 0
      orderOf TypeVar{} = 1
      orderOf TypeLevelString{} = 2
      orderOf TypeLevelInt{} = 3
      orderOf TypeWildcard{} = 4
      orderOf TypeConstructor{} = 5
      orderOf TypeOp{} = 6
      orderOf TypeApp{} = 7
      orderOf KindApp{} = 8
      orderOf ForAll{} = 9
      orderOf ConstrainedType{} = 10
      orderOf Skolem{} = 11
      orderOf REmpty{} = 12
      orderOf RCons{} = 13
      orderOf KindedType{} = 14
      orderOf BinaryNoParensType{} = 15
      orderOf ParensInType{} = 16

compareMaybeType :: Maybe (Type a) -> Maybe (Type b) -> Ordering
compareMaybeType (Just a) (Just b) = compareType a b
compareMaybeType Nothing Nothing = EQ
compareMaybeType Nothing _ = LT
compareMaybeType _ _ = GT

instance Eq (Constraint a) where
  (==) = eqConstraint

instance Ord (Constraint a) where
  compare = compareConstraint

eqConstraint :: Constraint a -> Constraint b -> Bool
eqConstraint (Constraint _ a b c d) (Constraint _ a' b' c' d') = a == a' && and (zipWith eqType b b') && and (zipWith eqType c c') && d == d'

compareConstraint :: Constraint a -> Constraint b -> Ordering
compareConstraint (Constraint _ a b c d) (Constraint _ a' b' c' d') = compare a a' <> fold (zipWith compareType b b') <> fold (zipWith compareType c c') <> compare d d'
