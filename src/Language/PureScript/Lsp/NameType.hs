module Language.PureScript.Lsp.NameType where

import Protolude
import Language.PureScript.Names

data LspNameType
  = IdentNameType
  | ValOpNameType
  | TyNameType
  | TyOpNameType
  | DctorNameType
  | TyClassNameType
  | ModNameType
  deriving (Show, Eq)


lspNameType :: Name -> LspNameType
lspNameType = \case 
  IdentName _ -> IdentNameType
  ValOpName _ -> ValOpNameType
  TyName _ -> TyNameType
  TyOpName _ -> TyOpNameType
  DctorName _ -> DctorNameType
  TyClassName _ -> TyClassNameType
  ModName _ -> ModNameType
