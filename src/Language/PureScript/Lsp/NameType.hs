{-# LANGUAGE DeriveAnyClass #-}

module Language.PureScript.Lsp.NameType where

import Database.SQLite.Simple.FromField (FromField (fromField))
import Database.SQLite.Simple.ToField (ToField (toField))
import Language.PureScript.Names
import Protolude

data LspNameType
  = IdentNameType
  | ValOpNameType
  | TyNameType
  | TyOpNameType
  | DctorNameType
  | TyClassNameType
  | ModNameType
  deriving (Show, Read, Eq, Generic)

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
