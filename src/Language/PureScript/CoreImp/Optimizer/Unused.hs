-- | Removes unused variables
module Language.PureScript.CoreImp.Optimizer.Unused
  ( removeCodeAfterReturnStatements
  , removeUndefinedApp
  , removeUnusedEffectFreeVars
  ) where

import Prelude

import Control.Monad (filterM)
import Data.Monoid (Any(..))
import Data.Set qualified as S
import Data.Text (Text)

import Language.PureScript.CoreImp.AST (AST(..), InitializerEffects(..), everything)
import Language.PureScript.Constants.Prim qualified as C

removeCodeAfterReturnStatements :: AST -> Maybe AST
removeCodeAfterReturnStatements (Block ss jss) =
  case break isReturn jss of
    (body, ret : _ : _) -> Just $ Block ss (body ++ [ret])
    _ -> Nothing
  where
  isReturn (Return _ _) = True
  isReturn (ReturnNoResult _) = True
  isReturn _ = False
removeCodeAfterReturnStatements _ = Nothing

removeUndefinedApp :: AST -> Maybe AST
removeUndefinedApp (App ss fn [Var _ C.S_undefined]) = Just $ App ss fn []
removeUndefinedApp _ = Nothing

removeUnusedEffectFreeVars :: [Text] -> [[AST]] -> [[AST]]
removeUnusedEffectFreeVars exps = loop
  where
  expsSet = S.fromList exps

  loop :: [[AST]] -> [[AST]]
  loop asts = if changed then loop (filter (not . null) asts') else asts
    where
    used = expsSet <> foldMap (foldMap (everything (<>) (\case Var _ x -> S.singleton x; _ -> S.empty))) asts
    (Any changed, asts') = traverse (filterM (anyFalses . isInUsedSet used)) asts

  isInUsedSet :: S.Set Text -> AST -> Bool
  isInUsedSet used = \case
    VariableIntroduction _ var (Just (NoEffects, _)) -> var `S.member` used
    _ -> True

  anyFalses :: Bool -> (Any, Bool)
  anyFalses x = (Any (not x), x)
