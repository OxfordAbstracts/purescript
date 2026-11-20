-- | This module optimizes code in the simplified-JavaScript intermediate representation.
--
-- The following optimizations are supported:
--
--  * Collapsing nested blocks
--
--  * Tail call elimination
--
--  * Inlining of (>>=) and ret for the Eff monad
--
--  * Removal of unnecessary thunks
--
--  * Eta conversion
--
--  * Inlining variables
--
--  * Inline Prelude.($), Prelude.(#), Prelude.(++), Prelude.(!!)
--
--  * Inlining primitive JavaScript operators
module Language.PureScript.CoreImp.Optimizer (optimize) where

import Prelude

import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)

import Control.Monad.Supply.Class (MonadSupply)
import Language.PureScript.CoreImp.AST (AST(..), InitializerEffects(..), anywhere, everywhere, everywhereTopDown, everywhereTopDownM)
import Language.PureScript.CoreImp.Optimizer.Blocks (collapseNestedBlocks, collapseNestedIfs)
import Language.PureScript.CoreImp.Optimizer.Common (applyAll)
import Language.PureScript.CoreImp.Optimizer.Inliner (etaConvert, evaluateIifes, inlineCommonOperators, inlineCommonValues, inlineFnComposition, inlineFnIdentity, inlineUnsafeCoerce, inlineUnsafePartial, inlineVariables, unThunk)
import Language.PureScript.CoreImp.Optimizer.MagicDo (inlineST, magicDoEff, magicDoEffect, magicDoST)
import Language.PureScript.CoreImp.Optimizer.TCO (tco)
import Language.PureScript.CoreImp.Optimizer.Unused (removeCodeAfterReturnStatements, removeUndefinedApp, removeUnusedEffectFreeVars)

-- | Apply a series of optimizer passes to simplified JavaScript code
optimize :: forall m. MonadSupply m => [Text] -> [[AST]] -> m [[AST]]
optimize exps jsDecls = removeUnusedEffectFreeVars exps <$> traverse (traverse go) jsDecls
  where
    go :: AST -> m AST
    go js = do
      js' <- untilFixedPointWhen inlineApplies inline js
      untilFixedPointWhen (anyRuleApplies tidyUpRules) (return . tidyUp) . tco . inlineST
        =<< magicDoUntilFixedPoint (magicDoST expander)
        =<< magicDoUntilFixedPoint (magicDoEff expander)
        =<< magicDoUntilFixedPoint (magicDoEffect expander) js'

    inline :: AST -> m AST
    inline = everywhereTopDownM (\js -> fromMaybe (return js) (inlineFnComposition expander js))
      . everywhereTopDown (orUnchanged (inlineFnIdentity expander))
      . everywhereTopDown (orUnchanged inlineUnsafeCoerce)
      . everywhereTopDown (orUnchanged inlineUnsafePartial)
      . tidyUp
      . everywhere (orUnchanged (inlineCommonValues expander))
      . everywhereTopDown (orUnchanged (inlineCommonOperators expander))

    inlineApplies :: AST -> Bool
    inlineApplies = anywhere $ \js ->
      isJust (inlineCommonOperators expander js)
        || isJust (inlineCommonValues expander js)
        || any (\rule -> isJust (rule js)) tidyUpRules
        || isJust (inlineUnsafePartial js)
        || isJust (inlineUnsafeCoerce js)
        || isJust (inlineFnIdentity expander js)
        || isJust (inlineFnComposition expander js :: Maybe (m AST))

    magicDoUntilFixedPoint :: (AST -> Maybe AST) -> AST -> m AST
    magicDoUntilFixedPoint rule = untilFixedPointWhen (anyRuleApplies [rule]) (return . everywhereTopDown (orUnchanged rule))

    tidyUp :: AST -> AST
    tidyUp = applyAll (map (everywhere . orUnchanged) tidyUpRules)

    tidyUpRules :: [AST -> Maybe AST]
    tidyUpRules =
      [ collapseNestedBlocks
      , collapseNestedIfs
      , removeCodeAfterReturnStatements
      , removeUndefinedApp
      , unThunk
      , etaConvert
      , evaluateIifes
      , inlineVariables
      ]

    expander = buildExpander (concat jsDecls)

untilFixedPoint :: (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go
  where
  go a = do
   a' <- f a
   if a' == a then return a' else go a'

untilFixedPointWhen :: (Monad m, Eq a) => (a -> Bool) -> (a -> m a) -> a -> m a
untilFixedPointWhen applies f = go
  where
  go a
    | applies a = do
        a' <- f a
        if a' == a then return a' else go a'
    | otherwise = return a

orUnchanged :: (AST -> Maybe AST) -> AST -> AST
orUnchanged rule js = fromMaybe js (rule js)

anyRuleApplies :: [AST -> Maybe AST] -> AST -> Bool
anyRuleApplies rules = anywhere (\js -> any (\rule -> isJust (rule js)) rules)

-- |
-- Take all top-level ASTs and return a function for expanding top-level
-- variables during the various inlining steps in `optimize`.
--
-- Everything that gets inlined as an optimization is of a form that would
-- have been lifted to a top-level binding during CSE, so for purposes of
-- inlining we can save some time by only expanding variables bound at that
-- level and not worrying about any inner scopes.
--
buildExpander :: [AST] -> AST -> AST
buildExpander asts
  | M.null vars = id
  | otherwise = everywhere replace
  where
  vars = M.fromListWith (\_ firstBound -> firstBound)
    [ (name, e) | VariableIntroduction _ name (Just (NoEffects, e)) <- asts ]
  replace v@(Var _ var) = M.findWithDefault v var vars
  replace other = other
