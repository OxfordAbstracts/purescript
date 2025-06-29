-- |
-- Functions and instances relating to unification
--
module Language.PureScript.TypeChecker.Unify
  ( freshType
  , freshTypeWithKind
  , solveType
  , substituteType
  , unknownsInType
  , unifyTypes
  , unifyRows
  , alignRowsWith
  , replaceTypeWildcards
  , varIfUnknown
  ) where

import Prelude

import Control.Monad (forM_, void, when)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..), gets, modify, state)
import Control.Monad.Writer.Class (MonadWriter(..))

import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.IntMap.Lazy qualified as IM
import Data.Text qualified as T

import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment qualified as E
import Language.PureScript.Errors (ErrorMessageHint(..), SimpleErrorMessage(..), SourceAnn, errorMessage, internalCompilerError, onErrorMessages, rethrow, warnWithPosition, withoutPosition)
import Language.PureScript.TypeChecker.Kinds (elaborateKind, instantiateKind, unifyKinds')
import Language.PureScript.TypeChecker.Monad (CheckState(..), Substitution(..), UnkLevel(..), Unknown, getLocalContext, guardWith, lookupUnkName, withErrorMessageHint, TypeCheckM)
import Language.PureScript.TypeChecker.Skolems (newSkolemConstant, skolemize)
import Language.PureScript.Types (Constraint(..), pattern REmptyKinded, RowListItem(..), SourceType, Type(..), WildcardData(..), alignRowsWith, everythingOnTypes, everywhereOnTypes, everywhereOnTypesM, getAnnForType, mkForAll, rowFromList, srcTUnknown)
import Data.Set qualified as S

-- | Generate a fresh type variable with an unknown kind. Avoid this if at all possible.
freshType :: TypeCheckM SourceType
freshType = state $ \st -> do
  let
    t = checkNextType st
    st' = st { checkNextType = t + 2
             , checkSubstitution =
                 (checkSubstitution st) { substUnsolved = IM.insert t (UnkLevel (pure t), E.kindType)
                                                        . IM.insert (t + 1) (UnkLevel (pure (t + 1)), srcTUnknown t)
                                                        . substUnsolved
                                                        $ checkSubstitution st
                                        }
             }
  (srcTUnknown (t + 1), st')

-- | Generate a fresh type variable with a known kind.
freshTypeWithKind :: SourceType -> TypeCheckM SourceType
freshTypeWithKind kind = state $ \st -> do
  let
    t = checkNextType st
    st' = st { checkNextType = t + 1
             , checkSubstitution =
                 (checkSubstitution st) { substUnsolved = IM.insert t (UnkLevel (pure t), kind) (substUnsolved (checkSubstitution st)) }
             }
  (srcTUnknown t, st')

-- | Update the substitution to solve a type constraint
solveType :: Int -> SourceType -> TypeCheckM ()
solveType u t = rethrow (onErrorMessages withoutPosition) $ do
  -- We strip the position so that any errors get rethrown with the position of
  -- the original unification constraint. Otherwise errors may arise from arbitrary
  -- locations. We don't otherwise have the "correct" position on hand, since it
  -- is maintained as part of the type-checker stack.
  occursCheck u t
  k1 <- elaborateKind t
  subst <- gets checkSubstitution
  k2 <- maybe (internalCompilerError ("No kind for unification variable ?" <> T.pack (show u))) (pure . substituteType subst . snd) . IM.lookup u . substUnsolved $ subst
  t' <- instantiateKind (t, k1) k2
  modify $ \cs -> cs { checkSubstitution =
                         (checkSubstitution cs) { substType =
                                                    IM.insert u t' $ substType $ checkSubstitution cs
                                                }
                     }

-- | Apply a substitution to a type
substituteType :: Substitution -> SourceType -> SourceType
substituteType sub = everywhereOnTypes go
  where
  go (TUnknown ann u) =
    case IM.lookup u (substType sub) of
      Nothing -> TUnknown ann u
      Just (TUnknown ann' u1) | u1 == u -> TUnknown ann' u1
      Just t -> substituteType sub t
  go other = other

-- | Make sure that an unknown does not occur in a type
occursCheck :: Int -> SourceType -> TypeCheckM ()
occursCheck _ TUnknown{} = return ()
occursCheck u t = void $ everywhereOnTypesM go t
  where
  go (TUnknown _ u') | u == u' = throwError . errorMessage . InfiniteType $ t
  go other = return other

-- | Compute a list of all unknowns appearing in a type
unknownsInType :: Type a -> [(a, Int)]
unknownsInType t = everythingOnTypes (.) go t []
  where
  go :: Type a -> [(a, Int)] -> [(a, Int)]
  go (TUnknown ann u) = ((ann, u) :)
  go _ = id

-- | Unify two types, updating the current substitution
unifyTypes :: SourceType -> SourceType -> TypeCheckM ()
unifyTypes t1 t2 | t1 == t2 = return ()
unifyTypes t1 t2 = do
  sub <- gets checkSubstitution
  withErrorMessageHint (ErrorUnifyingTypes t1 t2) $ unifyTypes'' (substituteType sub t1) (substituteType sub t2)
  where
  unifyTypes'' t1' t2'= do
    cache <- gets unificationCache
    when (S.notMember (t1', t2') cache) $ do
      modify $ \st -> st { unificationCache = S.insert (t1', t2') cache }
      unifyTypes' t1' t2'
  unifyTypes' (TUnknown _ u1) (TUnknown _ u2) | u1 == u2 = return ()
  unifyTypes' (TUnknown _ u) t = solveType u t
  unifyTypes' t (TUnknown _ u) = solveType u t
  unifyTypes' (ForAll ann1 _ ident1 mbK1 ty1 sc1) (ForAll ann2 _ ident2 mbK2 ty2 sc2) =
    case (sc1, sc2) of
      (Just sc1', Just sc2') -> do
        sko <- newSkolemConstant
        let sk1 = skolemize ann1 ident1 mbK1 sko sc1' ty1
        let sk2 = skolemize ann2 ident2 mbK2 sko sc2' ty2
        sk1 `unifyTypes` sk2
      _ -> internalError "unifyTypes: unspecified skolem scope"
  unifyTypes' (ForAll ann _ ident mbK ty1 (Just sc)) ty2 = do
    sko <- newSkolemConstant
    let sk = skolemize ann ident mbK sko sc ty1
    sk `unifyTypes` ty2
  unifyTypes' ForAll{} _ = internalError "unifyTypes: unspecified skolem scope"
  unifyTypes' ty f@ForAll{} = f `unifyTypes` ty
  unifyTypes' (TypeVar _ v1) (TypeVar _ v2) | v1 == v2 = return ()
  unifyTypes' ty1@(TypeConstructor _ c1) ty2@(TypeConstructor _ c2) =
    guardWith (errorMessage (TypesDoNotUnify ty1 ty2)) (c1 == c2)
  unifyTypes' (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = return ()
  unifyTypes' (TypeLevelInt    _ n1) (TypeLevelInt    _ n2) | n1 == n2 = return ()
  unifyTypes' (TypeApp _ t3 t4) (TypeApp _ t5 t6) = do
    t3 `unifyTypes` t5
    t4 `unifyTypes` t6
  unifyTypes' (KindApp _ t3 t4) (KindApp _ t5 t6) = do
    t3 `unifyKinds'` t5
    t4 `unifyTypes` t6
  unifyTypes' (Skolem _ _ _ s1 _) (Skolem _ _ _ s2 _) | s1 == s2 = return ()
  unifyTypes' (KindedType _ ty1 _) ty2 = ty1 `unifyTypes` ty2
  unifyTypes' ty1 (KindedType _ ty2 _) = ty1 `unifyTypes` ty2
  unifyTypes' r1@RCons{} r2 = unifyRows r1 r2
  unifyTypes' r1 r2@RCons{} = unifyRows r1 r2
  unifyTypes' r1@REmptyKinded{} r2 = unifyRows r1 r2
  unifyTypes' r1 r2@REmptyKinded{} = unifyRows r1 r2
  unifyTypes' (ConstrainedType _ c1 ty1) (ConstrainedType _ c2 ty2)
    | constraintClass c1 == constraintClass c2 && constraintData c1 == constraintData c2 = do
        traverse_ (uncurry unifyTypes) (constraintArgs c1 `zip` constraintArgs c2)
        ty1 `unifyTypes` ty2
  unifyTypes' ty1@ConstrainedType{} ty2 =
    throwError . errorMessage $ ConstrainedTypeUnified ty1 ty2
  unifyTypes' t3 t4@ConstrainedType{} = unifyTypes' t4 t3
  unifyTypes' t3 t4 =
    throwError . errorMessage $ TypesDoNotUnify t3 t4

-- | Unify two rows, updating the current substitution
--
-- Common labels are identified and unified. Remaining labels and types are unified with a
-- trailing row unification variable, if appropriate.
unifyRows :: SourceType -> SourceType -> TypeCheckM ()
unifyRows r1 r2 = sequence_ matches *> uncurry unifyTails rest where
  unifyTypesWithLabel l t1 t2 = withErrorMessageHint (ErrorInRowLabel l) $ unifyTypes t1 t2

  (matches, rest) = alignRowsWith unifyTypesWithLabel r1 r2

  unifyTails :: ([RowListItem SourceAnn], SourceType) -> ([RowListItem SourceAnn], SourceType) -> TypeCheckM ()
  unifyTails ([], TUnknown _ u)    (sd, r)               = solveType u (rowFromList (sd, r))
  unifyTails (sd, r)               ([], TUnknown _ u)    = solveType u (rowFromList (sd, r))
  unifyTails ([], REmptyKinded _ _) ([], REmptyKinded _ _) = return ()
  unifyTails ([], TypeVar _ v1)    ([], TypeVar _ v2)    | v1 == v2 = return ()
  unifyTails ([], Skolem _ _ _ s1 _) ([], Skolem _ _ _ s2 _) | s1 == s2 = return ()
  unifyTails (sd1, TUnknown a u1)  (sd2, TUnknown _ u2)  | u1 /= u2 = do
    forM_ sd1 $ occursCheck u2 . rowListType
    forM_ sd2 $ occursCheck u1 . rowListType
    rest' <- freshTypeWithKind =<< elaborateKind (TUnknown a u1)
    solveType u1 (rowFromList (sd2, rest'))
    solveType u2 (rowFromList (sd1, rest'))
  unifyTails _ _ =
    throwError . errorMessage $ TypesDoNotUnify r1 r2

-- |
-- Replace type wildcards with unknowns
--
replaceTypeWildcards :: SourceType -> TypeCheckM SourceType
replaceTypeWildcards = everywhereOnTypesM replace
  where
  replace (TypeWildcard ann wdata) = do
    t <- freshType
    ctx <- getLocalContext
    let err = case wdata of
          HoleWildcard n -> Just $ HoleInferredType n t ctx Nothing
          UnnamedWildcard -> Just $ WildcardInferredType t ctx
          IgnoredWildcard -> Nothing
    forM_ err $ warnWithPosition (fst ann) . tell . errorMessage
    return t
  replace other = return other

-- |
-- Replace outermost unsolved unification variables with named type variables
--
varIfUnknown :: [(Unknown, SourceType)] -> SourceType -> TypeCheckM SourceType
varIfUnknown unks ty = do
  bn' <- traverse toBinding unks
  ty' <- go ty
  pure $ mkForAll bn' ty'
  where
  toName :: Unknown -> TypeCheckM T.Text
  toName u = (<> T.pack (show u)) . fromMaybe "t" <$> lookupUnkName u

  toBinding :: (Unknown, SourceType) -> TypeCheckM (SourceAnn, (T.Text, Maybe SourceType))
  toBinding (u, k) = do
    u' <- toName u
    k' <- go k
    pure (getAnnForType ty, (u', Just k'))

  go :: SourceType -> TypeCheckM SourceType
  go = everywhereOnTypesM $ \case
    (TUnknown ann u) ->
      TypeVar ann <$> toName u
    t -> pure t
