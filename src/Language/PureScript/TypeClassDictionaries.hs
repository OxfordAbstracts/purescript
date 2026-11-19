module Language.PureScript.TypeClassDictionaries where

import Prelude

import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text, pack)

import Language.PureScript.AST.Declarations.ChainId (ChainId)
import Language.PureScript.Names (Ident, ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy, disqualify)
import Language.PureScript.Types (SourceConstraint, SourceType, Type(..))

--
-- Data representing a type class dictionary which is in scope
--
data TypeClassDictionaryInScope v
  = TypeClassDictionaryInScope {
    -- | The instance chain
      tcdChain :: Maybe ChainId
    -- | Index of the instance chain
    , tcdIndex :: Integer
    -- | The value with which the dictionary can be accessed at runtime
    , tcdValue :: v
    -- | How to obtain this instance via superclass relationships
    , tcdPath :: [(Qualified (ProperName 'ClassName), Integer)]
    -- | The name of the type class to which this type class instance applies
    , tcdClassName :: Qualified (ProperName 'ClassName)
    -- | Quantification of type variables in the instance head and dependencies
    , tcdForAll :: [(Text, SourceType)]
    -- | The kinds to which this type class instance applies
    , tcdInstanceKinds :: [SourceType]
    -- | The types to which this type class instance applies
    , tcdInstanceTypes :: [SourceType]
    -- | Type class dependencies which must be satisfied to construct this dictionary
    , tcdDependencies :: Maybe [SourceConstraint]
    -- | If this instance was unnamed, the type to use when describing it in
    -- error messages
    , tcdDescription :: Maybe SourceType
    }
    deriving (Show, Functor, Foldable, Traversable, Generic)

instance NFData v => NFData (TypeClassDictionaryInScope v)

type NamedDict = TypeClassDictionaryInScope (Qualified Ident)

type InstanceContext = M.Map QualifiedBy ModuleInstances
type ClassInstances = M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict))

data ModuleInstances = ModuleInstances
  { instancesByClass :: ClassInstances
  , instancesByHead :: M.Map (Qualified (ProperName 'ClassName)) (M.Map (Maybe (Qualified (ProperName 'TypeName))) [NamedDict])
  }

instance Show ModuleInstances where
  showsPrec n = showsPrec n . instancesByClass

instance NFData ModuleInstances where
  rnf = rnf . instancesByClass

instance Semigroup ModuleInstances where
  a <> b = moduleInstances (M.unionWith (M.unionWith (<>)) (instancesByClass a) (instancesByClass b))

moduleInstances :: ClassInstances -> ModuleInstances
moduleInstances classes = ModuleInstances classes (M.map index classes)
  where
  index dictMap = foldr (\dict -> M.insertWith (++) (key dict) [dict]) M.empty dicts
    where
    dicts = foldMap NEL.toList dictMap
    chainsWithElse = S.fromList [ch | dict <- dicts, tcdIndex dict > 0, Just ch <- [tcdChain dict]]
    key dict
      | Just ch <- tcdChain dict
      , S.notMember ch chainsWithElse
      , t : _ <- tcdInstanceTypes dict = headConstructor t
      | otherwise = Nothing

singletonInstance :: NamedDict -> InstanceContext
singletonInstance dict = M.singleton scope . moduleInstances . M.singleton (tcdClassName dict) $ M.singleton name (pure dict)
  where
  name@(Qualified scope _) = tcdValue dict

headConstructor :: Type a -> Maybe (Qualified (ProperName 'TypeName))
headConstructor = \case
  TypeConstructor _ c -> Just c
  TypeApp _ f _ -> headConstructor f
  KindApp _ f _ -> headConstructor f
  KindedType _ t _ -> headConstructor t
  _ -> Nothing

-- | Generate a name for a superclass reference which can be used in
-- generated code.
superclassName :: Qualified (ProperName 'ClassName) -> Integer -> Text
superclassName pn index = runProperName (disqualify pn) <> pack (show index)
