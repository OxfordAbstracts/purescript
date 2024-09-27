{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Language.PureScript.Lsp.Util where

import Codec.Serialise qualified as S
import Data.Text qualified as T
import Data.Text.Utf16.Rope.Mixed as Rope
import Database.SQLite.Simple.ToField (ToField (toField))
import Language.LSP.Protocol.Types (UInt)
import Language.LSP.Protocol.Types qualified as Types
import Language.PureScript.AST qualified as P
import Language.PureScript.AST.Declarations (declSourceAnn)
import Language.PureScript.Comments qualified as P
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Externs qualified as P
-- import Language.PureScript.Linter qualified as P
import Language.PureScript.Types qualified as P
import Protolude hiding (to)

posInSpan :: Types.Position -> Errors.SourceSpan -> Bool
posInSpan (Types.Position line col) (Errors.SourceSpan _ (Errors.SourcePos startLine startCol) (Errors.SourcePos endLine endCol)) =
  startLine <= fromIntegral (line + 1)
    && endLine >= fromIntegral (line + 1)
    && startCol <= fromIntegral (col + 1)
    && endCol >= fromIntegral (col + 1)

getDeclarationAtPos :: Types.Position -> [P.Declaration] -> Maybe P.Declaration
getDeclarationAtPos pos = find (posInSpan pos . fst . declSourceAnn)

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
      ch | isWordBreak ch -> idx
      _ -> getNextWs (idx + 1) txt

    getPrevWs :: Int -> Text -> Int
    getPrevWs 0 _ = 0
    getPrevWs idx txt = case T.index txt idx of
      ch | isWordBreak ch -> idx + 1
      _ -> getPrevWs (idx - 1) txt

    isWordBreak :: Char -> Bool
    isWordBreak = not . (isAlphaNum ||^ (== '_'))

-- getNameAtPosition :: (MonadIO m, MonadReader LspEnvironment m) => Types.Position -> m (Maybe P.Name)
-- getNameAtPosition pos = do
--   cacheMb <- cachedRebuild
--   case getDeclarationAtPos pos =<< P.getModuleDeclarations . currentModule <$> cacheMb of
--     Nothing -> pure Nothing
--     Just decl -> do
--       let name :: Maybe P.Name
--           name = P.everythingOnValues (<|>) getDeclarationName goDef goDef goDef goDef ^. _1 $ decl
--           goDef _ = Nothing
--           getDeclarationName :: P.Declaration -> Maybe P.Name
--           getDeclarationName = \case
--             _ -> Nothing

--       pure name
  -- where 
  --   getExprName :: P.Expr -> Maybe P.Name
  --   getExprName = \case
  --     P.Var _ q -> Just $ P.IdentName $ P.disqualify q
  --     _ -> Nothing

-- cacheMb
--   & maybe
--     (pure _)
--     \CurrentFile {..} -> do

--       -- let module' = P.efModule currentExterns
--       -- let decls = P.getModuleDeclarations module'
--       -- let file = P.efSource currentExterns
--       -- let word = getWordAt file Types.Position {..}
--       -- let decl = getDeclarationAtPos Types.Position {..} decls
--       -- let ident = P.Ident (P.IdentName $ P.Ident word)
--       -- pure $ P.IdentName ident
--       pure Nothing


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

