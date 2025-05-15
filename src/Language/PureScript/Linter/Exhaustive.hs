-- |
-- Module for exhaustivity checking over pattern matching definitions
-- The algorithm analyses the clauses of a definition one by one from top
-- to bottom, where in each step it has the cases already missing (uncovered),
-- and it generates the new set of missing cases.
module Language.PureScript.Linter.Exhaustive
  ( checkExhaustiveExpr,
  )
where

import Control.Applicative (Applicative (..))
import Control.Arrow (first, second)
import Control.Monad (unless, join)
import Control.Monad.Writer.Class (MonadWriter (..))
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Language.PureScript.AST.Binders (Binder (..))
import Language.PureScript.AST.Declarations (CaseAlternative (..), Expr (..), Guard (..), GuardedExpr (..), isTrueExpr, pattern MkUnguarded)
import Language.PureScript.AST.Literals (Literal (..))
import Language.PureScript.AST.Traversals (everywhereOnValuesM)
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (TypeKind (..))
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage (..), SourceSpan, errorMessage', pattern NullSourceAnn)
import Language.PureScript.Make.Index.Select (GetEnv)
import Language.PureScript.Names as P
import Language.PureScript.Pretty.Values (prettyPrintBinderAtom)
import Language.PureScript.TypeChecker.Monad (CheckState, lookupConstructorMb, lookupTypeMb)
import Language.PureScript.Types as P
import Protolude (MonadState, ifM, ordNub, foldlM)
import Prelude

-- | There are two modes of failure for the redundancy check:
--
-- 1. Exhaustivity was incomplete due to too many cases, so we couldn't determine redundancy.
-- 2. We didn't attempt to determine redundancy for a binder, e.g. an integer binder.
--
-- We want to warn the user in the first case.
data RedundancyError = Incomplete | Unknown

-- |
-- Qualifies a propername from a given qualified propername and a default module name
qualifyName ::
  ProperName a ->
  ModuleName ->
  Qualified (ProperName b) ->
  Qualified (ProperName a)
qualifyName n defmn qn = Qualified (ByModuleName mn) n
  where
    (mn, _) = qualify defmn qn

-- |
-- Given  a datatype or newtype name,
-- this function returns the associated data constructors if it is the case of a datatype
-- where: - ProperName is the name of the constructor (for example, "Nothing" in Maybe)
--        - [Type] is the list of arguments, if it has (for example, "Just" has [TypeVar "a"])
getConstructors :: forall m. (MonadState CheckState m, GetEnv m) => ModuleName -> Qualified (ProperName 'ConstructorName) -> m [(ProperName 'ConstructorName, [SourceType])]
getConstructors defmn n = do
  qpn <- getConsDataName n
  lnte <- lookupTypeMb qpn
  pure $ extractConstructors lnte
  where
    extractConstructors :: Maybe (SourceType, TypeKind) -> [(ProperName 'ConstructorName, [SourceType])]
    extractConstructors (Just (_, DataType _ _ pt)) = pt
    extractConstructors other = internalError $ "Data name not in the scope of the current environment in extractConstructors: " ++ show other 

    getConsDataName :: Qualified (ProperName 'ConstructorName) -> m (Qualified (ProperName 'TypeName))
    getConsDataName con =
      lookupConstructorMb con <&> \case
        Nothing -> internalError $ "Constructor " ++ T.unpack (showQualified runProperName con) ++ " not in the scope of the current environment in getConsDataName."
        Just (_, pm, _, _) -> qualifyName pm defmn con

-- getConsInfo :: Qualified (ProperName 'ConstructorName) -> m (Maybe (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
-- getConsInfo con = M.lookup con (dataConstructors env)

-- |
-- Replicates a wildcard binder
initialize :: Int -> [Binder]
initialize l = replicate l NullBinder

-- |
-- Applies a function over two lists of tuples that may lack elements
genericMerge ::
  (Ord a) =>
  (a -> Maybe b -> Maybe c -> d) ->
  [(a, b)] ->
  [(a, c)] ->
  [d]
genericMerge _ [] [] = []
genericMerge f bs [] = map (\(s, b) -> f s (Just b) Nothing) bs
genericMerge f [] bs = map (\(s, b) -> f s Nothing (Just b)) bs
genericMerge f bsl@((s, b) : bs) bsr@((s', b') : bs')
  | s < s' = f s (Just b) Nothing : genericMerge f bs bsr
  | s > s' = f s' Nothing (Just b') : genericMerge f bsl bs'
  | otherwise = f s (Just b) (Just b') : genericMerge f bs bs'

-- |
-- Find the uncovered set between two binders:
-- the first binder is the case we are trying to cover, the second one is the matching binder
missingCasesSingle :: (MonadState CheckState m, GetEnv m) => ModuleName -> Binder -> Binder -> m ([Binder], Either RedundancyError Bool)
missingCasesSingle _ _ NullBinder = pure ([], return True)
missingCasesSingle _ _ (VarBinder _ _) = pure ([], return True)
missingCasesSingle mn (VarBinder _ _) b = missingCasesSingle mn NullBinder b
missingCasesSingle mn br (NamedBinder _ _ bl) = missingCasesSingle mn br bl
missingCasesSingle mn NullBinder cb@(ConstructorBinder ss con _) = do
  ctrs <- getConstructors mn con
  let allPatterns =
        map (\(p, t) -> ConstructorBinder ss (qualifyName p mn con) (initialize $ length t)) ctrs
  binders <- join <$> traverse (\cp -> fst <$> missingCasesSingle mn cp cb) allPatterns
  return (binders, return True)
missingCasesSingle mn cb@(ConstructorBinder ss con bs) (ConstructorBinder _ con' bs')
  | con == con' = do
      (bs'', pr) <- missingCasesMultiple mn bs bs'
      pure (map (ConstructorBinder ss con) bs'', pr)
  | otherwise = return ([cb], return False)
missingCasesSingle mn NullBinder (LiteralBinder ss (ObjectLiteral bs)) = do
  (allMisses, pr) <- missingCasesMultiple mn (initialize $ length bs) (map snd bs)
  pure (map (LiteralBinder ss . ObjectLiteral . zip (map fst bs)) allMisses, pr)
missingCasesSingle mn (LiteralBinder _ (ObjectLiteral bs)) (LiteralBinder ss (ObjectLiteral bs')) = do
  (allMisses, pr) <- uncurry (missingCasesMultiple mn) (unzip binders)
  return (map (LiteralBinder ss . ObjectLiteral . zip sortedNames) allMisses, pr)
  where
    sortNames = sortOn fst

    (sbs, sbs') = (sortNames bs, sortNames bs')

    compB :: a -> Maybe a -> Maybe a -> (a, a)
    compB e b b' = (fm b, fm b')
      where
        fm = fromMaybe e

    compBS :: b -> a -> Maybe b -> Maybe b -> (a, (b, b))
    compBS e s b b' = (s, compB e b b')

    (sortedNames, binders) = unzip $ genericMerge (compBS NullBinder) sbs sbs'
missingCasesSingle _ NullBinder (LiteralBinder ss (BooleanLiteral b)) = return ([LiteralBinder ss . BooleanLiteral $ not b], return True)
missingCasesSingle _ (LiteralBinder ss (BooleanLiteral bl)) (LiteralBinder _ (BooleanLiteral br))
  | bl == br = return ([], return True)
  | otherwise = return ([LiteralBinder ss $ BooleanLiteral bl], return False)
missingCasesSingle mn b (PositionedBinder _ _ cb) = missingCasesSingle mn b cb
missingCasesSingle mn b (TypedBinder _ cb) = missingCasesSingle mn b cb
missingCasesSingle _ b _ = return ([b], Left Unknown)

-- |
-- Returns the uncovered set of binders
-- the first argument is the list of uncovered binders at step i
-- the second argument is the (i+1)th clause of a pattern matching definition
--
-- The idea of the algorithm is as follows:
-- it processes each binder of the two lists (say, `x` and `y`) one by one
-- at each step two cases arises:
--   - there are no missing cases between `x` and `y`: this is very straightforward, it continues with the remaining cases
--       but keeps the uncovered binder in its position.
--   - there are missing cases, let us call it the set `U`: on the one hand, we mix each new uncovered case in `U`
--       with the current tail of uncovered set. On the other hand, it continues with the remaining cases: here we
--       can use `x` (but it will generate overlapping cases), or `y`, which will generate non-overlapping cases.
--
--     As an example, consider:
--
--       data N = Z | S N
--       f Z Z = Z   --> {[S _, _], [Z, S _]} which are the right non-overlapping cases (GHC uses this).
--
--       if we use `x` instead of `y` (in this case, `y` stands for `Z` and `x` for `_`) we obtain:
--       f Z Z = Z   --> {[S _, _], [_, S _]} you can see that both cases overlaps each other.
--
--       Up to now, we've decided to use `x` just because we expect to generate uncovered cases which might be
--       redundant or not, but uncovered at least. If we use `y` instead, we'll need to have a redundancy checker
--       (which ought to be available soon), or increase the complexity of the algorithm.
missingCasesMultiple :: (MonadState CheckState m, GetEnv m) => ModuleName -> [Binder] -> [Binder] -> m ([[Binder]], Either RedundancyError Bool)
missingCasesMultiple mn = go
  where
    go (x : xs) (y : ys) = do
      (miss1, pr1) <- missingCasesSingle mn x y
      (miss2, pr2) <- go xs ys
      pure (map (: xs) miss1 ++ map (x :) miss2, liftA2 (&&) pr1 pr2)
    go _ _ = pure ([], pure True)

-- |
-- Guard handling
--
-- We say a guard is exhaustive iff it has an `otherwise` (or `true`) expression.
-- Example:
--   f x | x < 0 = 0
--       | otherwise = 1
--   is exhaustive, whereas `f x | x < 0` is not
--
-- or in case of a pattern guard if the pattern is exhaustive.
--
-- The function below say whether or not a guard has an `otherwise` expression
-- It is considered that `otherwise` is defined in Prelude
isExhaustiveGuard :: forall m. (GetEnv m, MonadState CheckState m) => ModuleName -> [GuardedExpr] -> m Bool
isExhaustiveGuard _ [MkUnguarded _] = pure True
isExhaustiveGuard moduleName gs =
  anyM (\(GuardedExpr grd _) -> isExhaustive grd) gs
  where
    isExhaustive :: [Guard] -> m Bool
    isExhaustive = allM checkGuard

    checkGuard :: Guard -> m Bool
    checkGuard (ConditionGuard cond) = pure $ isTrueExpr cond
    checkGuard (PatternGuard binder _) = do
      missing <- missingCasesSingle moduleName NullBinder binder
      pure $ case missing of
        ([], _) -> True -- there are no missing pattern for this guard
        _ -> False

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM f (x : xs) = do
  b <- f x
  if b then pure True else anyM f xs

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ [] = pure True
allM f (x : xs) = do
  b <- f x
  if b then allM f xs else pure False

-- |
-- Returns the uncovered set of case alternatives
missingCases :: (GetEnv m, MonadState CheckState m) => ModuleName -> [Binder] -> CaseAlternative -> m ([[Binder]], Either RedundancyError Bool)
missingCases mn uncovered ca = missingCasesMultiple mn uncovered (caseAlternativeBinders ca)

missingAlternative :: (GetEnv m, MonadState CheckState m) => ModuleName -> CaseAlternative -> [Binder] -> m ([[Binder]], Either RedundancyError Bool)
missingAlternative mn ca uncovered = do
  mcases <- missingCases mn uncovered ca
  ifM (isExhaustiveGuard mn (caseAlternativeResult ca)) (pure mcases) (pure ([uncovered], snd mcases))

-- |
-- Main exhaustivity checking function
-- Starting with the set `uncovered = { _ }` (nothing covered, one `_` for each function argument),
-- it partitions that set with the new uncovered cases, until it consumes the whole set of clauses.
-- Then, returns the uncovered set of case alternatives.
checkExhaustive ::
  forall m.
  (MonadWriter MultipleErrors m, GetEnv m, MonadState CheckState m) =>
  SourceSpan ->
  ModuleName ->
  Int ->
  [CaseAlternative] ->
  Expr ->
  m Expr
checkExhaustive ss mn numArgs cas expr = makeResult . first ordNub =<< foldlM step ([initialize numArgs], (pure True, [])) cas
  where
    step ::
      ([[Binder]], (Either RedundancyError Bool, [[Binder]])) ->
      CaseAlternative ->
      m ([[Binder]], (Either RedundancyError Bool, [[Binder]]))
    step (uncovered, (nec, redundant)) ca = do 
      (missed, pr) <- unzip <$> traverse (missingAlternative mn ca) uncovered
      let (missed', approx) = splitAt 10000 (ordNub (concat missed))
          cond = or <$> sequenceA pr

      -- let (missed, pr) = unzip (map (missingAlternative mn ca) uncovered)
      --     (missed', approx) = splitAt 10000 (ordNub (concat missed))
      --     cond = or <$> sequenceA pr
      pure ( missed',
            ( if null approx
                then liftA2 (&&) cond nec
                else Left Incomplete,
              if and cond
                then redundant
                else caseAlternativeBinders ca : redundant
            )
          )

    makeResult :: ([[Binder]], (Either RedundancyError Bool, [[Binder]])) -> m Expr
    makeResult (bss, (rr, bss')) =
      do
        unless (null bss') tellRedundant
        case rr of
          Left Incomplete -> tellIncomplete
          _ -> return ()
        return $
          if null bss
            then expr
            else addPartialConstraint (second null (splitAt 5 bss)) expr
      where
        tellRedundant = tell . errorMessage' ss . uncurry OverlappingPattern . second null . splitAt 5 $ bss'
        tellIncomplete = tell . errorMessage' ss $ IncompleteExhaustivityCheck

    -- We add a Partial constraint by annotating the expression to have type `Partial => _`.
    --
    -- The binder information is provided so that it can be embedded in the constraint,
    -- and then included in the error message.
    addPartialConstraint :: ([[Binder]], Bool) -> Expr -> Expr
    addPartialConstraint (bss, complete) e =
      TypedValue True e $
        srcConstrainedType (srcConstraint C.Partial [] [] (Just constraintData)) $
          TypeWildcard NullSourceAnn IgnoredWildcard
      where
        constraintData :: ConstraintData
        constraintData =
          PartialConstraintData (map (map prettyPrintBinderAtom) bss) complete

-- |
-- Exhaustivity checking
checkExhaustiveExpr ::
  forall m.
  (MonadWriter MultipleErrors m, GetEnv m, MonadState CheckState m) =>
  SourceSpan ->
  ModuleName ->
  Expr ->
  m Expr
checkExhaustiveExpr ss mn = onExpr'
  where
    (_, onExpr', _) = everywhereOnValuesM pure onExpr pure

    onExpr :: Expr -> m Expr
    onExpr e = case e of
      Case es cas ->
        checkExhaustive ss mn (length es) cas e
      _ ->
        pure e
