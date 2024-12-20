{-# LANGUAGE GADTs #-}

-- |
-- Functions for replacing fully applied type synonyms
module Language.PureScript.TypeChecker.Synonyms
  ( SynonymMap,
    KindMap,
    replaceAllTypeSynonyms,
  )
where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.State (MonadState)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.PureScript.Environment (TypeKind)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage (..), SourceSpan, errorMessage')
import Language.PureScript.Make.Index.Select (GetEnv)
import Language.PureScript.Names (ProperName, ProperNameType (..), Qualified)
import Language.PureScript.TypeChecker.Monad (CheckState, lookupSynonymMb, lookupTypeMb)
import Language.PureScript.Types (SourceType, Type (..), completeBinderList, everywhereOnTypesTopDownM, getAnnForType, replaceAllTypeVars)
import Prelude

-- | Type synonym information (arguments with kinds, aliased type), indexed by name
type SynonymMap = M.Map (Qualified (ProperName 'TypeName)) ([(Text, Maybe SourceType)], SourceType)

type KindMap = M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)

-- | Replace fully applied type synonyms
replaceAllTypeSynonyms :: forall e m. (e ~ MultipleErrors, MonadState CheckState m, GetEnv m, MonadError e m) => SourceType -> m SourceType
replaceAllTypeSynonyms = everywhereOnTypesTopDownM try
  where
    try :: SourceType -> m SourceType
    try t = fromMaybe t <$> go (fst $ getAnnForType t) 0 [] [] t

    go :: SourceSpan -> Int -> [SourceType] -> [SourceType] -> SourceType -> m (Maybe SourceType)
    go ss c kargs args (TypeConstructor _ ctor) = do
      lookupSynonymMb ctor >>= \case
        Just (synArgs, body)
          | c == length synArgs -> do
             kindArgs <- lookupKindArgs ctor
             if length kargs == length kindArgs  then
              let repl = replaceAllTypeVars (zip (map fst synArgs) args <> zip kindArgs kargs) body
               in Just <$> try repl
             else pure Nothing
          | length synArgs > c ->
              throwError . errorMessage' ss $ PartiallyAppliedSynonym ctor
        _ -> return Nothing
    go ss c kargs args (TypeApp _ f arg) = go ss (c + 1) kargs (arg : args) f
    go ss c kargs args (KindApp _ f arg) = go ss c (arg : kargs) args f
    go _ _ _ _ _ = return Nothing

    lookupKindArgs :: Qualified (ProperName 'TypeName) -> m [Text]
    lookupKindArgs ctor = do
      k <- lookupTypeMb ctor
      pure $ fromMaybe [] $ fmap (fmap (fst . snd) . fst) . completeBinderList . fst =<< k
