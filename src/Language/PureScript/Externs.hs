{-# Language DeriveAnyClass #-}
-- |
-- This module generates code for \"externs\" files, i.e. files containing only
-- foreign import declarations.
--
module Language.PureScript.Externs
  ( ExternsFile(..)
  , ExternsImport(..)
  , ExternsFixity(..)
  , ExternsTypeFixity(..)
  , ExternsDeclaration(..)
  , externsIsCurrentVersion
  , moduleToExternsFile
  , applyExternsFileToEnvironment
  , externsFileName
  , currentVersion
  ) where

import Prelude

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Control.Monad (join)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.List (foldl', find)
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Data.Map qualified as M
import Data.List.NonEmpty qualified as NEL
import GHC.Generics (Generic)

import Language.PureScript.AST (Associativity, Declaration(..), DeclarationRef(..), Fixity(..), ImportDeclarationType, Module(..), NameSource(..), Precedence, SourceSpan, pattern TypeFixityDeclaration, pattern ValueFixityDeclaration, getTypeOpRef, getValueOpRef)
import Language.PureScript.AST.Declarations.ChainId (ChainId)
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (DataDeclType, Environment(..), FunctionalDependency, NameKind(..), NameVisibility(..), TypeClassData(..), TypeKind(..), dictTypeName, makeTypeClassData)
import Language.PureScript.Names (Ident, ModuleName, OpName, OpNameType(..), ProperName, ProperNameType(..), Qualified(..), QualifiedBy(..), coerceProperName, isPlainIdent)
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope(..))
import Language.PureScript.Types (SourceConstraint, SourceType, srcInstanceType)

import Paths_purescript as Paths
import Data.Aeson (ToJSON, FromJSON)

-- | The data which will be serialized to an externs file
data ExternsFile = ExternsFile
  -- NOTE: Make sure to keep `efVersion` as the first field in this
  -- record, so the derived Serialise instance produces CBOR that can
  -- be checked for its version independent of the remaining format
  { efVersion :: Text
  -- ^ The externs version
  , efModuleName :: ModuleName
  -- ^ Module name
  , efExports :: [DeclarationRef]
  -- ^ List of module exports
  , efImports :: [ExternsImport]
  -- ^ List of module imports
  , efFixities :: [ExternsFixity]
  -- ^ List of operators and their fixities
  , efTypeFixities :: [ExternsTypeFixity]
  -- ^ List of type operators and their fixities
  , efDeclarations :: [ExternsDeclaration]
  -- ^ List of type and value declaration
  , efSourceSpan :: SourceSpan
  -- ^ Source span for error reporting
  } deriving (Show, Generic, NFData, ToJSON, FromJSON)

instance Serialise ExternsFile


-- | A module import in an externs file
data ExternsImport = ExternsImport
  {
  -- | The imported module
    eiModule :: ModuleName
  -- | The import type: regular, qualified or hiding
  , eiImportType :: ImportDeclarationType
  -- | The imported-as name, for qualified imports
  , eiImportedAs :: Maybe ModuleName
  } deriving (Show, Generic, NFData)

instance Serialise ExternsImport
instance ToJSON ExternsImport
instance FromJSON ExternsImport

-- | A fixity declaration in an externs file
data ExternsFixity = ExternsFixity
  {
  -- | The associativity of the operator
    efAssociativity :: Associativity
  -- | The precedence level of the operator
  , efPrecedence :: Precedence
  -- | The operator symbol
  , efOperator :: OpName 'ValueOpName
  -- | The value the operator is an alias for
  , efAlias :: Qualified (Either Ident (ProperName 'ConstructorName))
  } deriving (Eq, Show, Generic, NFData)

instance Serialise ExternsFixity
instance ToJSON ExternsFixity
instance FromJSON ExternsFixity

-- | A type fixity declaration in an externs file
data ExternsTypeFixity = ExternsTypeFixity
  {
  -- | The associativity of the operator
    efTypeAssociativity :: Associativity
  -- | The precedence level of the operator
  , efTypePrecedence :: Precedence
  -- | The operator symbol
  , efTypeOperator :: OpName 'TypeOpName
  -- | The value the operator is an alias for
  , efTypeAlias :: Qualified (ProperName 'TypeName)
  } deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

instance Serialise ExternsTypeFixity

-- | A type or value declaration appearing in an externs file
data ExternsDeclaration =
  -- | A type declaration
    EDType
      { edTypeName                :: ProperName 'TypeName
      , edTypeKind                :: SourceType
      , edTypeDeclarationKind     :: TypeKind
      }
  -- | A type synonym
  | EDTypeSynonym
      { edTypeSynonymName         :: ProperName 'TypeName
      , edTypeSynonymArguments    :: [(Text, Maybe SourceType)]
      , edTypeSynonymType         :: SourceType
      }
  -- | A data constructor
  | EDDataConstructor
      { edDataCtorName            :: ProperName 'ConstructorName
      , edDataCtorOrigin          :: DataDeclType
      , edDataCtorTypeCtor        :: ProperName 'TypeName
      , edDataCtorType            :: SourceType
      , edDataCtorFields          :: [Ident]
      }
  -- | A value declaration
  | EDValue
      { edValueName               :: Ident
      , edValueType               :: SourceType
      }
  -- | A type class declaration
  | EDClass
      { edClassName               :: ProperName 'ClassName
      , edClassTypeArguments      :: [(Text, Maybe SourceType)]
      , edClassMembers            :: [(Ident, SourceType)]
      , edClassConstraints        :: [SourceConstraint]
      , edFunctionalDependencies  :: [FunctionalDependency]
      , edIsEmpty                 :: Bool
      }
  -- | An instance declaration
  | EDInstance
      { edInstanceClassName       :: Qualified (ProperName 'ClassName)
      , edInstanceName            :: Ident
      , edInstanceForAll          :: [(Text, SourceType)]
      , edInstanceKinds           :: [SourceType]
      , edInstanceTypes           :: [SourceType]
      , edInstanceConstraints     :: Maybe [SourceConstraint]
      , edInstanceChain           :: Maybe ChainId
      , edInstanceChainIndex      :: Integer
      , edInstanceNameSource      :: NameSource
      , edInstanceSourceSpan      :: SourceSpan
      }
  deriving (Eq, Show, Generic, NFData, ToJSON, FromJSON)

instance Serialise ExternsDeclaration

currentVersion :: String
currentVersion = showVersion Paths.version

-- | Check whether the version in an externs file matches the currently running
-- version.
externsIsCurrentVersion :: ExternsFile -> Bool
externsIsCurrentVersion ef =
  T.unpack (efVersion ef) == currentVersion

-- | Convert an externs file back into a module
applyExternsFileToEnvironment :: ExternsFile -> Environment -> Environment
applyExternsFileToEnvironment ExternsFile{..} = flip (foldl' applyDecl) efDeclarations
  where
  applyDecl :: Environment -> ExternsDeclaration -> Environment
  applyDecl env (EDType pn kind tyKind) = env { types = M.insert (qual pn) (kind, tyKind) (types env) }
  applyDecl env (EDTypeSynonym pn args ty) = env { typeSynonyms = M.insert (qual pn) (args, ty) (typeSynonyms env) }
  applyDecl env (EDDataConstructor pn dTy tNm ty nms) = env { dataConstructors = M.insert (qual pn) (dTy, tNm, ty, nms) (dataConstructors env) }
  applyDecl env (EDValue ident ty) = env { names = M.insert (Qualified (ByModuleName efModuleName) ident) (ty, External, Defined) (names env) }
  applyDecl env (EDClass pn args members cs deps tcIsEmpty) = env { typeClasses = M.insert (qual pn) (makeTypeClassData args members cs deps tcIsEmpty) (typeClasses env) }
  applyDecl env (EDInstance className ident vars kinds tys cs ch idx ns ss) =
    env { typeClassDictionaries =
            updateMap
              (updateMap (M.insertWith (<>) (qual ident) (pure dict)) className)
              (ByModuleName efModuleName) (typeClassDictionaries env) }
    where
    dict :: NamedDict
    dict = TypeClassDictionaryInScope ch idx (qual ident) [] className vars kinds tys cs instTy

    updateMap :: (Ord k, Monoid a) => (a -> a) -> k -> M.Map k a -> M.Map k a
    updateMap f = M.alter (Just . f . fold)

    instTy :: Maybe SourceType
    instTy = case ns of
      CompilerNamed -> Just $ srcInstanceType ss vars className tys
      UserNamed -> Nothing

  qual :: a -> Qualified a
  qual = Qualified (ByModuleName efModuleName)

-- | Generate an externs file for all declarations in a module.
--
-- The `Map Ident Ident` argument should contain any top-level `GenIdent`s that
-- were rewritten to `Ident`s when the module was compiled; this rewrite only
-- happens in the CoreFn, not the original module AST, so it needs to be
-- applied to the exported names here also. (The appropriate map is returned by
-- `L.P.Renamer.renameInModule`.)
moduleToExternsFile :: Module -> Environment -> M.Map Ident Ident -> ExternsFile
moduleToExternsFile (Module _ _ _ _ Nothing) _ _ = internalError "moduleToExternsFile: module exports were not elaborated"
moduleToExternsFile (Module ss _ mn ds (Just exps)) env renamedIdents = ExternsFile{..}
  where
  efVersion       = T.pack currentVersion
  efModuleName    = mn
  efExports       = map renameRef exps
  efImports       = mapMaybe importDecl ds
  efFixities      = mapMaybe fixityDecl ds
  efTypeFixities  = mapMaybe typeFixityDecl ds
  efDeclarations  = concatMap toExternsDeclaration exps
  efSourceSpan    = ss

  fixityDecl :: Declaration -> Maybe ExternsFixity
  fixityDecl (ValueFixityDeclaration _ (Fixity assoc prec) name op) =
    fmap (const (ExternsFixity assoc prec op name)) (find ((== Just op) . getValueOpRef) exps)
  fixityDecl _ = Nothing

  typeFixityDecl :: Declaration -> Maybe ExternsTypeFixity
  typeFixityDecl (TypeFixityDeclaration _ (Fixity assoc prec) name op) =
    fmap (const (ExternsTypeFixity assoc prec op name)) (find ((== Just op) . getTypeOpRef) exps)
  typeFixityDecl _ = Nothing

  importDecl :: Declaration -> Maybe ExternsImport
  importDecl (ImportDeclaration _ m mt qmn) = Just (ExternsImport m mt qmn)
  importDecl _ = Nothing

  toExternsDeclaration :: DeclarationRef -> [ExternsDeclaration]
  toExternsDeclaration (TypeRef _ pn dctors) =
    case Qualified (ByModuleName mn) pn `M.lookup` types env of
      Nothing -> internalError "toExternsDeclaration: no kind in toExternsDeclaration"
      Just (kind, TypeSynonym)
        | Just (args, synTy) <- Qualified (ByModuleName mn) pn `M.lookup` typeSynonyms env -> [ EDType pn kind TypeSynonym, EDTypeSynonym pn args synTy ]
      Just (kind, ExternData rs) -> [ EDType pn kind (ExternData rs) ]
      Just (kind, tk@(DataType _ _ tys)) ->
        EDType pn kind tk : [ EDDataConstructor dctor dty pn ty args
                            | dctor <- fromMaybe (map fst tys) dctors
                            , (dty, _, ty, args) <- maybeToList (Qualified (ByModuleName mn) dctor `M.lookup` dataConstructors env)
                            ]
      _ -> internalError "toExternsDeclaration: Invalid input"
  toExternsDeclaration (ValueRef _ ident)
    | Just (ty, _, _) <- Qualified (ByModuleName mn) ident `M.lookup` names env
    = [ EDValue (lookupRenamedIdent ident) ty ]
  toExternsDeclaration (TypeClassRef _ className)
    | let dictName = dictTypeName . coerceProperName $ className
    , Just TypeClassData{..} <- Qualified (ByModuleName mn) className `M.lookup` typeClasses env
    , Just (kind, tk) <- Qualified (ByModuleName mn) (coerceProperName className) `M.lookup` types env
    , Just (dictKind, dictData@(DataType _ _ [(dctor, _)])) <- Qualified (ByModuleName mn) dictName `M.lookup` types env
    , Just (dty, _, ty, args) <- Qualified (ByModuleName mn) dctor `M.lookup` dataConstructors env
    = [ EDType (coerceProperName className) kind tk
      , EDType dictName dictKind dictData
      , EDDataConstructor dctor dty dictName ty args
      , EDClass className typeClassArguments ((\(a, b, _) -> (a, b)) <$> typeClassMembers) typeClassSuperclasses typeClassDependencies typeClassIsEmpty
      ]
  toExternsDeclaration (TypeInstanceRef ss' ident ns)
    = [ EDInstance tcdClassName (lookupRenamedIdent ident) tcdForAll tcdInstanceKinds tcdInstanceTypes tcdDependencies tcdChain tcdIndex ns ss'
      | m1 <- maybeToList (M.lookup (ByModuleName mn) (typeClassDictionaries env))
      , m2 <- M.elems m1
      , nel <- maybeToList (M.lookup (Qualified (ByModuleName mn) ident) m2)
      , TypeClassDictionaryInScope{..} <- NEL.toList nel
      ]
  toExternsDeclaration _ = []

  renameRef :: DeclarationRef -> DeclarationRef
  renameRef = \case
    ValueRef ss' ident -> ValueRef ss' $ lookupRenamedIdent ident
    TypeInstanceRef ss' ident _ | not $ isPlainIdent ident -> TypeInstanceRef ss' (lookupRenamedIdent ident) CompilerNamed
    other -> other

  lookupRenamedIdent :: Ident -> Ident
  lookupRenamedIdent = flip (join M.findWithDefault) renamedIdents

externsFileName :: FilePath
externsFileName = "externs.cbor"
