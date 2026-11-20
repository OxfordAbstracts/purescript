{-# LANGUAGE GADTs #-}

-- |
-- Functions for replacing fully applied type synonyms
--
module Language.PureScript.TypeChecker.Synonyms
  ( SynonymMap
  , KindMap
  , replaceAllTypeSynonyms
  ) where

import Prelude

import Control.Monad.Error.Class (MonadError(..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Map qualified as M
import Data.Text (Text)
import Language.PureScript.Environment (Environment(..), TypeKind)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), SourceSpan, errorMessage')
import Language.PureScript.Names (ProperName, ProperNameType(..), Qualified)
import Language.PureScript.TypeChecker.Monad (getEnv, TypeCheckM)
import Language.PureScript.Types (SourceType, Type(..), Constraint(..), completeBinderList, everythingOnTypes, getAnnForType, replaceAllTypeVars)

-- | Type synonym information (arguments with kinds, aliased type), indexed by name
type SynonymMap = M.Map (Qualified (ProperName 'TypeName)) ([(Text, Maybe SourceType)], SourceType)

type KindMap = M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)

replaceAllTypeSynonyms'
  :: SynonymMap
  -> KindMap
  -> SourceType
  -> Either MultipleErrors SourceType
replaceAllTypeSynonyms' syns kinds ty
  | mentionsSynonym ty = fromMaybe ty <$> expand False ty
  | otherwise = Right ty
  where
  mentionsSynonym :: SourceType -> Bool
  mentionsSynonym = everythingOnTypes (||) $ \case
    TypeConstructor _ ctor -> M.member ctor syns
    _ -> False

  expand :: Bool -> SourceType -> Either MultipleErrors (Maybe SourceType)
  expand headIsNotSynonym t
    | headIsNotSynonym = descend True t
    | otherwise = case spineHead t of
        TypeConstructor _ ctor | M.member ctor syns ->
          go (fst $ getAnnForType t) 0 [] [] t >>= \case
            Just repl -> Just . fromMaybe repl <$> expand False repl
            Nothing -> descend False t
        _ -> descend True t

  descend :: Bool -> SourceType -> Either MultipleErrors (Maybe SourceType)
  descend headIsNotSynonym = \case
    TypeApp ann t1 t2 -> both (TypeApp ann) (expand headIsNotSynonym t1) t1 (expand False t2) t2
    KindApp ann t1 t2 -> both (KindApp ann) (expand headIsNotSynonym t1) t1 (expand False t2) t2
    ForAll ann vis arg mbK t sco -> case mbK of
      Just k -> both (\k' t' -> ForAll ann vis arg (Just k') t' sco) (expand False k) k (expand False t) t
      Nothing -> one (\t' -> ForAll ann vis arg Nothing t' sco) <$> expand False t
    ConstrainedType ann c t -> do
      kargs <- expandList (constraintKindArgs c)
      args <- expandList (constraintArgs c)
      t' <- expand False t
      pure $! case (kargs, args, t') of
        (Nothing, Nothing, Nothing) -> Nothing
        _ -> Just $! ConstrainedType ann
          (c { constraintKindArgs = fromMaybe (constraintKindArgs c) kargs, constraintArgs = fromMaybe (constraintArgs c) args })
          (fromMaybe t t')
    Skolem ann name mbK i sc -> case mbK of
      Just k -> one (\k' -> Skolem ann name (Just k') i sc) <$> expand False k
      Nothing -> pure Nothing
    RCons ann name t rest -> both (RCons ann name) (expand False t) t (expand False rest) rest
    KindedType ann t k -> both (KindedType ann) (expand False t) t (expand False k) k
    BinaryNoParensType ann t1 t2 t3 -> do
      t1' <- expand False t1
      t2' <- expand False t2
      t3' <- expand False t3
      pure $! case (t1', t2', t3') of
        (Nothing, Nothing, Nothing) -> Nothing
        _ -> Just $! BinaryNoParensType ann (fromMaybe t1 t1') (fromMaybe t2 t2') (fromMaybe t3 t3')
    ParensInType ann t -> one (ParensInType ann) <$> expand False t
    _ -> pure Nothing

  both :: (SourceType -> SourceType -> SourceType) -> Either MultipleErrors (Maybe SourceType) -> SourceType -> Either MultipleErrors (Maybe SourceType) -> SourceType -> Either MultipleErrors (Maybe SourceType)
  both k ma a mb b = do
    a' <- ma
    b' <- mb
    pure $! case (a', b') of
      (Nothing, Nothing) -> Nothing
      (Just x, Nothing) -> Just $! k x b
      (Nothing, Just y) -> Just $! k a y
      (Just x, Just y) -> Just $! k x y

  one :: (SourceType -> SourceType) -> Maybe SourceType -> Maybe SourceType
  one k = \case
    Nothing -> Nothing
    Just t -> Just $! k t

  expandList :: [SourceType] -> Either MultipleErrors (Maybe [SourceType])
  expandList ts = do
    ts' <- traverse (expand False) ts
    pure $! if all isNothing ts' then Nothing else Just (zipWith fromMaybe ts ts')

  spineHead :: SourceType -> SourceType
  spineHead (TypeApp _ f _) = spineHead f
  spineHead (KindApp _ f _) = spineHead f
  spineHead t = t

  go :: SourceSpan -> Int -> [SourceType] -> [SourceType] -> SourceType -> Either MultipleErrors (Maybe SourceType)
  go ss c kargs args (TypeConstructor _ ctor)
    | Just (synArgs, body) <- M.lookup ctor syns
    , c == length synArgs
    , kindArgs <- lookupKindArgs ctor
    , length kargs == length kindArgs
    = pure $ Just $ replaceAllTypeVars (zip (map fst synArgs) args <> zip kindArgs kargs) body
    | Just (synArgs, _) <- M.lookup ctor syns
    , length synArgs > c
    = throwError . errorMessage' ss $ PartiallyAppliedSynonym ctor
  go ss c kargs args (TypeApp _ f arg) = go ss (c + 1) kargs (arg : args) f
  go ss c kargs args (KindApp _ f arg) = go ss c (arg : kargs) args f
  go _ _ _ _ _ = return Nothing

  lookupKindArgs :: Qualified (ProperName 'TypeName) -> [Text]
  lookupKindArgs ctor = fromMaybe [] $ fmap (fmap (fst . snd) . fst) . completeBinderList . fst =<< M.lookup ctor kinds

-- | Replace fully applied type synonyms
replaceAllTypeSynonyms :: SourceType -> TypeCheckM SourceType
replaceAllTypeSynonyms d = do
  env <- getEnv
  either throwError return $ replaceAllTypeSynonyms' (typeSynonyms env) (types env) d
