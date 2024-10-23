{-# LANGUAGE DeriveAnyClass #-}

module Language.PureScript.Lsp.NameType where

import Database.SQLite.Simple.FromField (FromField (fromField))
import Database.SQLite.Simple.ToField (ToField (toField))
import Language.PureScript.Names
import Protolude
import Language.PureScript.Externs (ExternsDeclaration(..))
import Data.Aeson qualified as A

data LspNameType
  = IdentNameType
  | ValOpNameType
  | TyNameType
  | TyOpNameType
  | DctorNameType
  | TyClassNameType
  | ModNameType
  deriving (Show, Read, Eq, Generic, A.ToJSON, A.FromJSON)


readableType :: LspNameType -> Text
readableType = \case
  IdentNameType -> "Value"
  ValOpNameType -> "Operator"
  TyNameType -> "Type"
  TyOpNameType -> "Type Operator"
  DctorNameType -> "Constructor"
  TyClassNameType -> "Type Class"
  ModNameType -> "Module"

instance ToField LspNameType where
  toField = toField . (show :: LspNameType -> Text)

instance FromField LspNameType where
  fromField = fmap (fromMaybe IdentNameType . (readMaybe :: Text -> Maybe LspNameType)) . fromField

lspNameType :: Name -> LspNameType
lspNameType = \case
  IdentName _ -> IdentNameType
  ValOpName _ -> ValOpNameType
  TyName _ -> TyNameType
  TyOpName _ -> TyOpNameType
  DctorName _ -> DctorNameType
  TyClassName _ -> TyClassNameType
  ModName _ -> ModNameType

externDeclNameType :: ExternsDeclaration -> LspNameType
externDeclNameType = \case
  EDType _ _ _ -> TyNameType
  EDTypeSynonym _ _ _ -> TyNameType
  EDDataConstructor _ _ _ _ _ -> DctorNameType
  EDValue _ _ -> IdentNameType
  EDClass _ _ _ _ _ _ -> TyClassNameType
  EDInstance _ _ _ _ _ _ _ _ _ _ -> IdentNameType
