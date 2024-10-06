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
import Control.Monad.Except (ExceptT, runExceptT, MonadTrans (lift))
import Control.Monad.State (MonadState)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.PureScript.Environment (TypeKind)
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage (..), SourceSpan, errorMessage')
import Language.PureScript.Names (ProperName, ProperNameType (..), Qualified)
import Language.PureScript.TypeChecker.Monad (CheckState, getTypeSynonym, getType)
import Language.PureScript.Types (SourceType, Type (..), completeBinderList, everywhereOnTypesTopDownM, getAnnForType, replaceAllTypeVars)
import Prelude

-- | Type synonym information (arguments with kinds, aliased type), indexed by name
type SynonymMap = M.Map (Qualified (ProperName 'TypeName)) ([(Text, Maybe SourceType)], SourceType)

type KindMap = M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)

-- | Replace fully applied type synonyms
replaceAllTypeSynonyms :: forall m e. (e ~ MultipleErrors, MonadState (CheckState m) m, MonadError e m) => SourceType -> m SourceType
replaceAllTypeSynonyms d = do
  either throwError return =<< runExceptT (replaceAllTypeSynonyms' d)
  where
    replaceAllTypeSynonyms' :: SourceType -> ExceptT MultipleErrors m SourceType
    replaceAllTypeSynonyms' = everywhereOnTypesTopDownM try

    try :: SourceType -> ExceptT MultipleErrors m SourceType
    try t = fromMaybe t <$> go (fst $ getAnnForType t) 0 [] [] t

    go :: SourceSpan -> Int -> [SourceType] -> [SourceType] -> SourceType -> ExceptT MultipleErrors m (Maybe SourceType)
    go  ss c kargs args (TypeConstructor _ ctor) = do
      synMb <- lift $ getTypeSynonym ctor
      kindArgs <- lookupKindArgs ctor
      case synMb of
        Just (synArgs, body) | c == length synArgs && length kargs == length kindArgs -> do
          let repl = replaceAllTypeVars (zip (map fst synArgs) args <> zip kindArgs kargs) body
          Just <$> try repl
        Just (synArgs, _) | length synArgs > c -> throwError . errorMessage' ss $ PartiallyAppliedSynonym ctor
        _ -> return Nothing

    go ss c kargs args (TypeApp _ f arg) = go ss (c + 1) kargs (arg : args) f
    go ss c kargs args (KindApp _ f arg) = go ss c (arg : kargs) args f
    go _ _ _ _ _ = return Nothing

    lookupKindArgs :: Qualified (ProperName 'TypeName) ->  ExceptT MultipleErrors m [Text]
    lookupKindArgs ctor = do
      kindMb <- lift $ getType ctor
      case kindMb of
        Just (kind, _) -> return $ maybe [] (fmap (fst . snd) . fst) (completeBinderList kind)
        _ -> return []

