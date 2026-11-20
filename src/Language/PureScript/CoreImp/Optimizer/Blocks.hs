-- | Optimizer steps for simplifying JavaScript blocks
module Language.PureScript.CoreImp.Optimizer.Blocks
  ( collapseNestedBlocks
  , collapseNestedIfs
  ) where

import Prelude

import Language.PureScript.CoreImp.AST (AST(..), BinaryOperator(..))

-- | Collapse blocks which appear nested directly below another block
collapseNestedBlocks :: AST -> Maybe AST
collapseNestedBlocks (Block ss sts) | any isBlock sts = Just $ Block ss (concatMap go sts)
  where
  isBlock Block{} = True
  isBlock _ = False

  go :: AST -> [AST]
  go (Block _ sts') = sts'
  go s = [s]
collapseNestedBlocks _ = Nothing

collapseNestedIfs :: AST -> Maybe AST
collapseNestedIfs (IfElse _ (BooleanLiteral _ True) (Block _ [js]) _) = Just js
collapseNestedIfs (IfElse s1 cond1 (Block _ [IfElse s2 cond2 body Nothing]) Nothing) =
  Just $ IfElse s1 (Binary s2 And cond1 cond2) body Nothing
collapseNestedIfs _ = Nothing
