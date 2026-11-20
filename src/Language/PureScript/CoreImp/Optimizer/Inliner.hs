-- | This module performs basic inlining of known functions
module Language.PureScript.CoreImp.Optimizer.Inliner
  ( inlineVariables
  , inlineCommonValues
  , inlineCommonOperators
  , inlineFnComposition
  , inlineFnIdentity
  , inlineUnsafeCoerce
  , inlineUnsafePartial
  , etaConvert
  , unThunk
  , evaluateIifes
  ) where

import Prelude

import Control.Monad.Supply.Class (MonadSupply, freshName)

import Data.Either (rights)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Language.PureScript.Names (ModuleName)
import Language.PureScript.PSString (PSString, mkString)
import Language.PureScript.CoreImp.AST (AST(..), BinaryOperator(..), InitializerEffects(..), UnaryOperator(..), getSourceSpan)
import Language.PureScript.CoreImp.Optimizer.Common (pattern Ref, applyRules, isAnyReassignedOrUpdated, isReassigned, isRebound, replaceIdent, replaceIdents, variablesIn)
import Language.PureScript.AST (SourceSpan(..))
import Language.PureScript.Constants.Libs qualified as C
import Language.PureScript.Constants.Prim qualified as C

-- TODO: Potential bug:
-- Shouldn't just inline this case: { var x = 0; x.toFixed(10); }
-- Needs to be: { 0..toFixed(10); }
-- Probably needs to be fixed in pretty-printer instead.
shouldInline :: AST -> Bool
shouldInline (Var _ _) = True
shouldInline (ModuleAccessor _ _ _) = True
shouldInline (NumericLiteral _ _) = True
shouldInline (StringLiteral _ _) = True
shouldInline (BooleanLiteral _ _) = True
shouldInline (Indexer _ index val) = shouldInline index && shouldInline val
shouldInline _ = False

etaConvert :: AST -> Maybe AST
etaConvert (Block ss [Return _ (App _ (Function _ Nothing idents block@(Block _ body)) args)])
  | all shouldInline args &&
    not (any ((`isRebound` block) . Var Nothing) idents) &&
    not (any (`isRebound` block) args)
    = Just $ Block ss (map (replaceIdents (zip idents args)) body)
etaConvert (Function _ Nothing [] (Block _ [Return _ (App _ fn [])])) = Just fn
etaConvert _ = Nothing

unThunk :: AST -> Maybe AST
unThunk (Block ss jss@(_ : _)) =
  case last jss of
    Return _ (App _ (Function _ Nothing [] (Block _ body)) []) -> Just $ Block ss $ init jss ++ body
    _ -> Nothing
unThunk _ = Nothing

evaluateIifes :: AST -> Maybe AST
evaluateIifes (App _ (Function _ Nothing [] (Block _ [Return _ ret])) []) = Just ret
evaluateIifes (App _ (Function _ Nothing idents (Block _ [Return ss ret])) [])
  | not (any (`isReassigned` ret) idents) = Just $ replaceIdents (map (, Var ss C.S_undefined) idents) ret
evaluateIifes _ = Nothing

inlineVariables :: AST -> Maybe AST
inlineVariables (Block ss sts) = Block ss <$> inlineFirst sts
  where
  inlineFirst :: [AST] -> Maybe [AST]
  inlineFirst [] = Nothing
  inlineFirst (s : rest) = case inlineInto s rest of
    Just rest' -> Just (inlineRest rest')
    Nothing -> (s :) <$> inlineFirst rest

  inlineRest :: [AST] -> [AST]
  inlineRest rest = fromMaybe rest (inlineFirst rest)

  inlineInto :: AST -> [AST] -> Maybe [AST]
  inlineInto (VariableIntroduction _ var (Just (_, js))) rest
    | shouldInline js && not (any (isAnyReassignedOrUpdated (var : variablesIn js)) rest) =
      Just (map (replaceIdent var js) rest)
  inlineInto _ _ = Nothing
inlineVariables _ = Nothing

inlineCommonValues :: (AST -> AST) -> AST -> Maybe AST
inlineCommonValues expander = convert
  where
  convert :: AST -> Maybe AST
  convert (expander -> App ss (Ref fn) [Ref dict])
    | dict `elem` [C.P_semiringNumber, C.P_semiringInt], C.P_zero <- fn = Just $ NumericLiteral ss (Left 0)
    | dict `elem` [C.P_semiringNumber, C.P_semiringInt], C.P_one <- fn = Just $ NumericLiteral ss (Left 1)
    | C.P_boundedBoolean <- dict, C.P_bottom <- fn = Just $ BooleanLiteral ss False
    | C.P_boundedBoolean <- dict, C.P_top <- fn = Just $ BooleanLiteral ss True
  convert (App ss (expander -> App _ (Ref C.P_negate) [Ref C.P_ringInt]) [x])
    = Just $ Binary ss BitwiseOr (Unary ss Negate x) (NumericLiteral ss (Left 0))
  convert (App ss (App _ (expander -> App _ (Ref fn) [Ref dict]) [x]) [y])
    | C.P_semiringInt <- dict, C.P_add <- fn = Just $ intOp ss Add x y
    | C.P_semiringInt <- dict, C.P_mul <- fn = Just $ intOp ss Multiply x y
    | C.P_ringInt <- dict, C.P_sub <- fn = Just $ intOp ss Subtract x y
  convert _ = Nothing
  intOp ss op x y = Binary ss BitwiseOr (Binary ss op x y) (NumericLiteral ss (Left 0))

inlineCommonOperators :: (AST -> AST) -> AST -> Maybe AST
inlineCommonOperators expander = onSingleArgument . applyRules $
  [ binary C.P_semiringNumber C.P_add Add
  , binary C.P_semiringNumber C.P_mul Multiply

  , binary C.P_ringNumber C.P_sub Subtract
  , unary  C.P_ringNumber C.P_negate Negate

  , binary C.P_euclideanRingNumber C.P_div Divide

  , binary C.P_eqNumber C.P_eq EqualTo
  , binary C.P_eqNumber C.P_notEq NotEqualTo
  , binary C.P_eqInt C.P_eq EqualTo
  , binary C.P_eqInt C.P_notEq NotEqualTo
  , binary C.P_eqString C.P_eq EqualTo
  , binary C.P_eqString C.P_notEq NotEqualTo
  , binary C.P_eqChar C.P_eq EqualTo
  , binary C.P_eqChar C.P_notEq NotEqualTo
  , binary C.P_eqBoolean C.P_eq EqualTo
  , binary C.P_eqBoolean C.P_notEq NotEqualTo

  , binary C.P_ordBoolean C.P_lessThan LessThan
  , binary C.P_ordBoolean C.P_lessThanOrEq LessThanOrEqualTo
  , binary C.P_ordBoolean C.P_greaterThan GreaterThan
  , binary C.P_ordBoolean C.P_greaterThanOrEq GreaterThanOrEqualTo
  , binary C.P_ordChar C.P_lessThan LessThan
  , binary C.P_ordChar C.P_lessThanOrEq LessThanOrEqualTo
  , binary C.P_ordChar C.P_greaterThan GreaterThan
  , binary C.P_ordChar C.P_greaterThanOrEq GreaterThanOrEqualTo
  , binary C.P_ordInt C.P_lessThan LessThan
  , binary C.P_ordInt C.P_lessThanOrEq LessThanOrEqualTo
  , binary C.P_ordInt C.P_greaterThan GreaterThan
  , binary C.P_ordInt C.P_greaterThanOrEq GreaterThanOrEqualTo
  , binary C.P_ordNumber C.P_lessThan LessThan
  , binary C.P_ordNumber C.P_lessThanOrEq LessThanOrEqualTo
  , binary C.P_ordNumber C.P_greaterThan GreaterThan
  , binary C.P_ordNumber C.P_greaterThanOrEq GreaterThanOrEqualTo
  , binary C.P_ordString C.P_lessThan LessThan
  , binary C.P_ordString C.P_lessThanOrEq LessThanOrEqualTo
  , binary C.P_ordString C.P_greaterThan GreaterThan
  , binary C.P_ordString C.P_greaterThanOrEq GreaterThanOrEqualTo

  , binary C.P_semigroupString C.P_append Add

  , binary C.P_heytingAlgebraBoolean C.P_conj And
  , binary C.P_heytingAlgebraBoolean C.P_disj Or
  , unary  C.P_heytingAlgebraBoolean C.P_not Not

  , binary' C.P_or BitwiseOr
  , binary' C.P_and BitwiseAnd
  , binary' C.P_xor BitwiseXor
  , binary' C.P_shl ShiftLeft
  , binary' C.P_shr ShiftRight
  , binary' C.P_zshr ZeroFillShiftRight
  , unary'  C.P_complement BitwiseNot

  , inlineNonClassFunction (isModFnWithDict C.P_unsafeIndex) $ flip (Indexer Nothing)
  , inlineUncurried
  ]
  where
  onSingleArgument f js@(App _ _ [_]) = f js
  onSingleArgument _ _ = Nothing

  inlineUncurried js
    | hasUncurriedHead (maxArity + 1) js = uncurriedRules js
    | otherwise = Nothing

  hasUncurriedHead _ (ModuleAccessor _ mn _) = mn `elem`
    [ C.M_Data_Function_Uncurried
    , C.M_Effect_Uncurried
    , C.M_Control_Monad_Eff_Uncurried
    , C.M_Control_Monad_ST_Uncurried
    ]
  hasUncurriedHead n (App _ f [_]) | n > 0 = hasUncurriedHead (n - 1) f
  hasUncurriedHead _ _ = False

  maxArity = 10 :: Int
  uncurriedRules = applyRules $
    [ fn | i <- [0..maxArity], fn <- [ mkFn i, runFn i ] ] ++
    [ fn | i <- [0..maxArity], fn <- [ mkEffFn C.P_mkEffFn i, runEffFn C.P_runEffFn i ] ] ++
    [ fn | i <- [0..maxArity], fn <- [ mkEffFn C.P_mkEffectFn i, runEffFn C.P_runEffectFn i ] ] ++
    [ fn | i <- [0..maxArity], fn <- [ mkEffFn C.P_mkSTFn i, runEffFn C.P_runSTFn i ] ]

  binary :: (ModuleName, PSString) -> (ModuleName, PSString) -> BinaryOperator -> AST -> Maybe AST
  binary dict fn op = convert where
    convert :: AST -> Maybe AST
    convert (App ss (App _ (expander -> App _ (Ref fn') [Ref dict']) [x]) [y]) | dict == dict', fn == fn' = Just $ Binary ss op x y
    convert _ = Nothing
  binary' :: (ModuleName, PSString) -> BinaryOperator -> AST -> Maybe AST
  binary' fn op = convert where
    convert :: AST -> Maybe AST
    convert (App ss (App _ (Ref fn') [x]) [y]) | fn == fn' = Just $ Binary ss op x y
    convert _ = Nothing
  unary :: (ModuleName, PSString) -> (ModuleName, PSString) -> UnaryOperator -> AST -> Maybe AST
  unary dict fn op = convert where
    convert :: AST -> Maybe AST
    convert (App ss (expander -> App _ (Ref fn') [Ref dict']) [x]) | dict == dict', fn == fn' = Just $ Unary ss op x
    convert _ = Nothing
  unary' :: (ModuleName, PSString) -> UnaryOperator -> AST -> Maybe AST
  unary' fn op = convert where
    convert :: AST -> Maybe AST
    convert (App ss (Ref fn') [x]) | fn == fn' = Just $ Unary ss op x
    convert _ = Nothing

  mkFn :: Int -> AST -> Maybe AST
  mkFn = mkFn' C.P_mkFn $ \ss1 ss2 ss3 args js ->
    Function ss1 Nothing args (Block ss2 [Return ss3 js])

  mkEffFn :: (ModuleName, PSString) -> Int -> AST -> Maybe AST
  mkEffFn mkFn_ = mkFn' mkFn_ $ \ss1 ss2 ss3 args js ->
    Function ss1 Nothing args (Block ss2 [Return ss3 (App ss3 js [])])

  mkFn' :: (ModuleName, PSString) -> (Maybe SourceSpan -> Maybe SourceSpan -> Maybe SourceSpan -> [Text] -> AST -> AST) -> Int -> AST -> Maybe AST
  mkFn' mkFn_ res 0 = convert where
    convert :: AST -> Maybe AST
    convert (App _ (Ref mkFnN) [Function s1 Nothing [_] (Block s2 [Return s3 js])]) | isNFn mkFn_ 0 mkFnN =
      Just $ res s1 s2 s3 [] js
    convert _ = Nothing
  mkFn' mkFn_ res n = convert where
    convert :: AST -> Maybe AST
    convert (App ss (Ref mkFnN) [fn]) | isNFn mkFn_ n mkFnN =
      case collectArgs n [] fn of
        Just (args, [Return ss' ret]) -> Just $ res ss ss ss' args ret
        _ -> Nothing
    convert _ = Nothing
    collectArgs :: Int -> [Text] -> AST -> Maybe ([Text], [AST])
    collectArgs 1 acc (Function _ Nothing [oneArg] (Block _ js)) | length acc == n - 1 = Just (reverse (oneArg : acc), js)
    collectArgs m acc (Function _ Nothing [oneArg] (Block _ [Return _ ret])) = collectArgs (m - 1) (oneArg : acc) ret
    collectArgs _ _   _ = Nothing

  isNFn :: (ModuleName, PSString) -> Int -> (ModuleName, PSString) -> Bool
  isNFn prefix n fn = fmap (<> mkString (T.pack $ show n)) prefix == fn

  runFn :: Int -> AST -> Maybe AST
  runFn = runFn' C.P_runFn App

  runEffFn :: (ModuleName, PSString) -> Int -> AST -> Maybe AST
  runEffFn runFn_ = runFn' runFn_ $ \ss fn acc ->
    Function ss Nothing [] (Block ss [Return ss (App ss fn acc)])

  runFn' :: (ModuleName, PSString) -> (Maybe SourceSpan -> AST -> [AST] -> AST) -> Int -> AST -> Maybe AST
  runFn' runFn_ res n = convert where
    convert :: AST -> Maybe AST
    convert = go n []

    go :: Int -> [AST] -> AST -> Maybe AST
    go 0 acc (App ss (Ref runFnN) [fn]) | isNFn runFn_ n runFnN && length acc == n =
      Just $ res ss fn acc
    go m acc (App _ lhs [arg]) = go (m - 1) (arg : acc) lhs
    go _ _   _ = Nothing

  inlineNonClassFunction :: (AST -> Bool) -> (AST -> AST -> AST) -> AST -> Maybe AST
  inlineNonClassFunction p f = convert where
    convert :: AST -> Maybe AST
    convert (App _ (App _ op' [x]) [y]) | p op' = Just $ f x y
    convert _ = Nothing

  isModFnWithDict :: (ModuleName, PSString) -> AST -> Bool
  isModFnWithDict fn (App _ (Ref fn') [Var _ _]) = fn == fn'
  isModFnWithDict _ _ = False

-- (f <<< g $ x) = f (g x)
-- (f <<< g)     = \x -> f (g x)
inlineFnComposition :: forall m. MonadSupply m => (AST -> AST) -> AST -> Maybe (m AST)
inlineFnComposition expander = convert
  where
  convert :: AST -> Maybe (m AST)
  convert (App s1 (App s2 (App _ (expander -> App _ (Ref fn) [Ref C.P_semigroupoidFn]) [x]) [y]) [z])
    | C.P_compose <- fn = Just . return $ App s1 x [App s2 y [z]]
    | C.P_composeFlipped <- fn = Just . return $ App s2 y [App s1 x [z]]
  convert app@(App ss (App _ (expander -> App _ (Ref fn) [Ref C.P_semigroupoidFn]) _) _)
    | fn `elem` [C.P_compose, C.P_composeFlipped] = Just $ mkApps ss <$> goApps app <*> freshName
  convert _ = Nothing

  mkApps :: Maybe SourceSpan -> [Either AST (Text, AST)] -> Text -> AST
  mkApps ss fns a = App ss (Function ss Nothing [] (Block ss $ vars <> [Return Nothing comp])) []
    where
    vars = uncurry (VariableIntroduction ss) . fmap (Just . (UnknownEffects, )) <$> rights fns
    comp = Function ss Nothing [a] (Block ss [Return Nothing apps])
    apps = foldr (\fn acc -> App ss (mkApp fn) [acc]) (Var ss a) fns

  mkApp :: Either AST (Text, AST) -> AST
  mkApp = either id $ \(name, arg) -> Var (getSourceSpan arg) name

  goApps :: AST -> m [Either AST (Text, AST)]
  goApps (App _ (App _ (expander -> App _ (Ref fn) [Ref C.P_semigroupoidFn]) [x]) [y])
    | C.P_compose <- fn = mappend <$> goApps x <*> goApps y
    | C.P_composeFlipped <- fn = mappend <$> goApps y <*> goApps x
  goApps app@App {} = pure . Right . (,app) <$> freshName
  goApps other = pure [Left other]

inlineFnIdentity :: (AST -> AST) -> AST -> Maybe AST
inlineFnIdentity expander = convert
  where
  convert :: AST -> Maybe AST
  convert (App _ (expander -> App _ (Ref C.P_identity) [Ref C.P_categoryFn]) [x]) = Just x
  convert _ = Nothing

inlineUnsafeCoerce :: AST -> Maybe AST
inlineUnsafeCoerce (App _ (Ref C.P_unsafeCoerce) [ comp ]) = Just comp
inlineUnsafeCoerce _ = Nothing

inlineUnsafePartial :: AST -> Maybe AST
inlineUnsafePartial (App ss (Ref C.P_unsafePartial) [ comp ]) = Just $ App ss comp [ Var ss C.S_undefined ]
inlineUnsafePartial _ = Nothing
