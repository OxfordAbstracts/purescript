{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Language.PureScript.Lsp.Print where

import Control.Lens (Field1 (_1), (^.))
import Data.Text qualified as T
import Language.PureScript.AST qualified as P
import Language.PureScript.AST.Traversals (accumTypes)
import Language.PureScript.Externs qualified as P
-- import Language.PureScript.Linter qualified as P
import Language.PureScript.Names qualified as P
import Language.PureScript.Pretty qualified as P
import Language.PureScript.Types qualified as P
import Protolude hiding (to)

printDeclarationType :: P.Declaration -> Text
printDeclarationType decl =
  Protolude.fold $
    (head :: [Text] -> Maybe Text) $
      accumTypes (pure . T.pack . P.prettyPrintType maxBound) ^. _1 $
        decl

printDeclarationTypeMb :: P.Declaration -> Maybe Text
printDeclarationTypeMb decl =
  (head :: [Text] -> Maybe Text) $
    accumTypes (pure . T.pack . P.prettyPrintType maxBound) ^. _1 $
      decl

printName :: P.Name -> Text
printName = \case
  P.IdentName ident -> P.runIdent ident
  P.ValOpName op -> P.runOpName op
  P.TyName name -> P.runProperName name
  P.TyOpName op -> P.runOpName op
  P.DctorName name -> P.runProperName name
  P.TyClassName name -> P.runProperName name
  P.ModName name -> P.runModuleName name

printEfDeclName :: P.ExternsDeclaration -> Text
printEfDeclName = \case
  P.EDType name _ _ -> P.runProperName name
  P.EDTypeSynonym name _ _ -> P.runProperName name
  P.EDDataConstructor name _ _ _ _ -> P.runProperName name
  P.EDValue ident _ -> P.runIdent ident
  P.EDClass name _ _ _ _ _ -> P.runProperName name
  P.EDInstance name _ _ _ _ _ _ _ _ _ -> P.runProperName $ P.disqualify name

printEfDeclType :: P.ExternsDeclaration -> Text
printEfDeclType =
  \case
    P.EDType _ ty _ -> T.pack $ P.prettyPrintType maxBound ty
    P.EDTypeSynonym _ _ ty -> T.pack $ P.prettyPrintType maxBound ty
    P.EDDataConstructor _ _ _ ty _ -> T.pack $ P.prettyPrintType maxBound ty
    P.EDValue _ ty -> T.pack $ P.prettyPrintType maxBound ty
    P.EDClass {..} ->
      let constraints :: [P.SourceConstraint] -> P.Type () -> P.Type ()
          constraints [] t = t
          constraints (sc : scs) t = P.ConstrainedType () (void sc) (constraints scs t)

          args :: [(Text, Maybe P.SourceType)] -> P.Type () -> P.Type ()
          args [] t = t
          args ((n, Nothing) : ts) t = P.TypeApp () (P.TypeVar () n) (args ts t)
          args ((n, Just ty) : ts) t = P.TypeApp () (P.KindedType () (P.TypeVar () n) (void ty)) (args ts t)
       in T.pack $
            P.prettyPrintType maxBound $
              constraints edClassConstraints $
                args edClassTypeArguments $
                  P.TypeVar () "Constraint"
    _ -> "instance"
