{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Language.PureScript.Lsp.Util where

import Codec.Serialise qualified as S
import Control.Lens (Field1 (_1), (^.))
import Data.Text qualified as T
import Data.Text.Utf16.Rope.Mixed as Rope
import Database.SQLite.Simple.ToField (ToField (toField))
import Language.LSP.Protocol.Types (UInt)
import Language.LSP.Protocol.Types qualified as Types
import Language.PureScript (accumTypes)
import Language.PureScript qualified as P
import Language.PureScript.AST.Declarations (declSourceAnn)
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Ide.Error (prettyPrintTypeSingleLine)
import Protolude hiding (to)

posInSpan :: Types.Position -> Errors.SourceSpan -> Bool
posInSpan (Types.Position line col) (Errors.SourceSpan _ (Errors.SourcePos startLine startCol) (Errors.SourcePos endLine endCol)) =
  startLine <= fromIntegral (line + 1)
    && endLine >= fromIntegral (line + 1)
    && startCol <= fromIntegral (col + 1)
    && endCol >= fromIntegral (col + 1)

getDeclarationAtPos :: Types.Position -> [P.Declaration] -> Maybe P.Declaration
getDeclarationAtPos pos = find (posInSpan pos . fst . declSourceAnn)

printDeclarationType :: P.Declaration -> Text
printDeclarationType decl =
  Protolude.fold $
    (head :: [Text] -> Maybe Text) $
      accumTypes (pure . T.pack . P.prettyPrintType maxBound) ^. _1 $
        decl

getWordAt :: Rope -> Types.Position -> Text
getWordAt file Types.Position {..} =
  let (_, after) = splitAtLine (fromIntegral _line) file
      (ropeLine, _) = splitAtLine 1 after
      line' = Rope.toText ropeLine
   in getWordOnLine line' _character

getWordOnLine :: Text -> UInt -> Text
getWordOnLine line' col =
  let start = getPrevWs (fromIntegral col) line'
      end = getNextWs (fromIntegral col) line'
   in T.strip $ T.take (end - start) $ T.drop start line'
  where
    getNextWs :: Int -> Text -> Int
    getNextWs idx txt | idx >= T.length txt = idx
    getNextWs idx txt = case T.index txt idx of
      ch | isSpace ch -> idx
      _ -> getNextWs (idx + 1) txt

    getPrevWs :: Int -> Text -> Int
    getPrevWs 0 _ = 0
    getPrevWs idx txt = case T.index txt idx of
      ch | isSpace ch -> idx + 1
      _ -> getPrevWs (idx - 1) txt

efDeclName :: P.ExternsDeclaration -> Text
efDeclName = \case
  P.EDType name _ _ -> P.runProperName name
  P.EDTypeSynonym name _ _ -> P.runProperName name
  P.EDDataConstructor name _ _ _ _ -> P.runProperName name
  P.EDValue ident _ -> P.runIdent ident
  P.EDClass name _ _ _ _ _ _ -> P.runProperName name
  P.EDInstance name _ _ _ _ _ _ _ _ _ -> P.runProperName $ P.disqualify name

data ExternsDeclarationCategory
  = EDCType
  | EDCTypeSynonym
  | EDCDataConstructor
  | EDCValue
  | EDCClass
  | EDCInstance
  deriving (Eq, Show, Read, Generic, S.Serialise)

instance ToField ExternsDeclarationCategory where
  toField = toField . S.serialise

efDeclCategory :: P.ExternsDeclaration -> ExternsDeclarationCategory
efDeclCategory = \case
  P.EDType {} -> EDCType
  P.EDTypeSynonym {} -> EDCTypeSynonym
  P.EDDataConstructor {} -> EDCDataConstructor
  P.EDValue {} -> EDCValue
  P.EDClass {} -> EDCClass
  P.EDInstance {} -> EDCInstance

efDeclSourceType :: P.ExternsDeclaration -> P.SourceType
efDeclSourceType = \case
  P.EDType _ ty _ -> ty
  P.EDTypeSynonym _ _ ty -> ty
  P.EDDataConstructor _ _ _ ty _ -> ty
  P.EDValue _ ty -> ty
  P.EDClass {} -> P.srcREmpty
  P.EDInstance {} -> P.srcREmpty

efDeclSourceSpan :: P.ExternsDeclaration -> P.SourceSpan
efDeclSourceSpan = \case
  P.EDClass _ _ _ _ _ _ span -> span
  P.EDInstance _ _ _ _ _ _ _ _ _ span -> span
  ed ->
    fromMaybe P.nullSourceSpan $ foldr (\(ss, _) _ -> Just ss) Nothing (efDeclSourceType ed)

efDeclComments :: P.ExternsDeclaration -> [P.Comment]
efDeclComments = foldr getComments [] . efDeclSourceType
  where
    getComments :: Errors.SourceAnn -> [P.Comment] -> [P.Comment]
    getComments (_, cs) acc = cs ++ acc

printName :: P.Name -> Text
printName = \case
  P.IdentName ident -> P.runIdent ident
  P.ValOpName op -> P.runOpName op
  P.TyName name -> P.runProperName name
  P.TyOpName op -> P.runOpName op
  P.DctorName name -> P.runProperName name
  P.TyClassName name -> P.runProperName name
  P.ModName name -> P.runModuleName name

printSourceType :: P.SourceType -> Text
printSourceType = prettyPrintTypeSingleLine