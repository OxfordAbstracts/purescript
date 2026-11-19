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

import Control.Monad ((>=>))
import Control.Monad.State.Strict (StateT, modify', runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Map qualified as M
import Data.Text (Text)
import Debug.Trace (traceEvent)

import Control.Monad.Supply.Class (MonadSupply)
import Language.PureScript.CoreImp.AST (AST(..), InitializerEffects(..), everything, everywhere)
import Language.PureScript.CoreImp.Optimizer.Blocks (collapseNestedBlocks, collapseNestedIfs)
import Language.PureScript.CoreImp.Optimizer.Inliner (etaConvert, evaluateIifes, inlineCommonOperators, inlineCommonValues, inlineFnComposition, inlineFnIdentity, inlineUnsafeCoerce, inlineUnsafePartial, inlineVariables, unThunk)
import Language.PureScript.CoreImp.Optimizer.MagicDo (inlineST, magicDoEff, magicDoEffect, magicDoST)
import Language.PureScript.CoreImp.Optimizer.TCO (tco)
import Language.PureScript.CoreImp.Optimizer.Unused (removeCodeAfterReturnStatements, removeUndefinedApp, removeUnusedEffectFreeVars)

data PassCount = PassCount !Int !Int !Int

type Passes m = StateT (M.Map String PassCount) m

-- | Apply a series of optimizer passes to simplified JavaScript code
optimize :: forall m. MonadSupply m => String -> [Text] -> [[AST]] -> m [[AST]]
optimize modName exps jsDecls = do
  (result, counts) <- runStateT (traverse (traverse go) jsDecls) M.empty
  let rows = [ unwords [name, show calls, show changed, show nodes]
             | (name, PassCount calls changed nodes) <- M.toAscList counts ]
  traceEvent (unwords ("jsopt" : modName : rows)) $ pure $ removeUnusedEffectFreeVars exps result
  where
    go :: AST -> Passes m AST
    go = fixed "inline.loop" inliners
      >=> fixed "magic.Effect" (pure . magicDoEffect expander)
      >=> fixed "magic.Eff" (pure . magicDoEff expander)
      >=> fixed "magic.ST" (pure . magicDoST expander)
      >=> pass "inlineST" inlineST
      >=> pass "tco" tco
      >=> fixed "late.loop" (tidy "late.")

    inliners = pass "operators" (inlineCommonOperators expander)
      >=> pass "values" (inlineCommonValues expander)
      >=> tidy "early."
      >=> pass "unsafePartial" inlineUnsafePartial
      >=> pass "unsafeCoerce" inlineUnsafeCoerce
      >=> pass "identity" (inlineFnIdentity expander)
      >=> (fmap fst . observed "composition" (lift . inlineFnComposition expander))

    tidy phase = pass (phase <> "inlineVars") inlineVariables
      >=> pass (phase <> "iifes") evaluateIifes
      >=> pass (phase <> "eta") etaConvert
      >=> pass (phase <> "unThunk") unThunk
      >=> pass (phase <> "undefined") removeUndefinedApp
      >=> pass (phase <> "afterReturn") removeCodeAfterReturnStatements
      >=> pass (phase <> "ifs") collapseNestedIfs
      >=> pass (phase <> "blocks") collapseNestedBlocks

    pass name f = fmap fst . observed name (pure . f)

    fixed name f = loop where
      loop a = do
        (b, changed) <- observed name f a
        if changed then loop b else pure b

    expander = buildExpander (concat jsDecls)

observed :: Monad m => String -> (AST -> Passes m AST) -> AST -> Passes m (AST, Bool)
observed name f a = do
  b <- f a
  let changed = b /= a
      count = PassCount 1 (fromEnum changed) (everything (+) (const 1) a)
      add (PassCount x y z) (PassCount i j k) = PassCount (x + i) (y + j) (z + k)
  modify' $ M.insertWith add name count
  pure (b, changed)

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
