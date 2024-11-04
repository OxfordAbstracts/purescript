{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Lsp.Handlers.Hover (hoverHandler) where

import Control.Lens ((^.))
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript qualified as P
import Language.PureScript.Docs.Convert.Single (convertComments)
import Language.PureScript.Ide.Error (prettyPrintTypeSingleLine)
import Language.PureScript.Lsp.AtPosition (binderSourceSpan, spanToRange)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationTypeInModule)
import Language.PureScript.Lsp.Docs (readDeclarationDocsWithNameType)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.State (cachedRebuild)
import Language.PureScript.Lsp.Types (OpenFile (..))
import Language.PureScript.Lsp.Util (positionToSourcePos)
import Language.PureScript.TypeChecker.IdeArtifacts (IdeArtifact (..), IdeArtifactValue (..), getArtifactsAtPosition, smallestArtifact)
import Protolude hiding (handle, to)
import Text.PrettyPrint.Boxes (render)

hoverHandler :: Server.Handlers HandlerM
hoverHandler = Server.requestHandler Message.SMethod_TextDocumentHover $ \req res -> do
  let Types.HoverParams docIdent startPos _prog = req ^. LSP.params
      filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri

      nullRes = res $ Right $ Types.InR Types.Null

      markdownRes range md =
        res $ Right $ Types.InL $ Types.Hover (Types.InL $ Types.MarkupContent Types.MarkupKind_Markdown md) range

      forLsp :: Maybe a -> (a -> HandlerM ()) -> HandlerM ()
      forLsp val f = maybe nullRes f val

      lookupExprTypes :: P.Expr -> HandlerM [Text]
      lookupExprTypes = \case
        P.Var _ (P.Qualified (P.ByModuleName modName) ident) -> do
          fmap (showTypeSection modName (P.runIdent ident)) <$> getAstDeclarationTypeInModule (Just IdentNameType) modName (P.runIdent ident)
        P.Op _ (P.Qualified (P.ByModuleName modName) op) -> do
          fmap (showTypeSection modName (P.runOpName op)) <$> getAstDeclarationTypeInModule (Just ValOpNameType) modName (P.runOpName op)
        P.Constructor _ (P.Qualified (P.ByModuleName modName) dctor) -> do
          fmap (showTypeSection modName (P.runProperName dctor)) <$> getAstDeclarationTypeInModule (Just DctorNameType) modName (P.runProperName dctor)
        P.TypedValue _ e _ | not (generatedExpr e) -> do
          lookupExprTypes e
        P.PositionedValue _ _ e | not (generatedExpr e) -> do
          lookupExprTypes e
        _ -> pure []

      lookupExprDocs :: P.Expr -> HandlerM (Maybe Text)
      lookupExprDocs = \case
        P.Var _ (P.Qualified (P.ByModuleName modName) ident) -> do
          readDeclarationDocsWithNameType modName IdentNameType (P.runIdent ident)
        P.Op _ (P.Qualified (P.ByModuleName modName) op) -> do
          readDeclarationDocsWithNameType modName ValOpNameType (P.runOpName op)
        P.Constructor _ (P.Qualified (P.ByModuleName modName) dctor) -> do
          readDeclarationDocsWithNameType modName DctorNameType (P.runProperName dctor)
        _ -> pure Nothing
  forLsp filePathMb \filePath -> do
    cacheOpenMb <- cachedRebuild filePath
    debugLsp $ "Cache found: " <> show (isJust cacheOpenMb)
    forLsp cacheOpenMb \OpenFile {..} -> do
      let atPos = getArtifactsAtPosition (positionToSourcePos startPos) (P.checkIdeArtifacts ofEndCheckState)
      debugLsp $ "hover artiacts length: " <> show (length atPos)
      case smallestArtifact (negate . artifactInterest) atPos of
        Just (IdeArtifact {..}) ->
          case iaValue of
            IaExpr _ expr -> do
              let inferredRes = pursTypeStr (dispayExprOnHover expr) (Just $ prettyPrintTypeSingleLine iaType) []
              foundTypes <- lookupExprTypes expr
              docs <- lookupExprDocs expr
              markdownRes (Just $ spanToRange iaSpan) $
                joinMarkup
                  [ Just inferredRes,
                    head foundTypes,
                    showDocs <$> docs
                  ]
            IaTypeName name -> do
              let name' = P.runProperName name
                  inferredRes = pursTypeStr name' (Just $ prettyPrintTypeSingleLine iaType) []
                  modName = fromMaybe ofModuleName iaDefinitionModule
              docs <- readDeclarationDocsWithNameType modName TyNameType name'
              foundTypes <- getAstDeclarationTypeInModule (Just TyNameType) modName name'
              markdownRes (Just $ spanToRange iaSpan) $
                joinMarkup
                  [ Just inferredRes,
                    showTypeSection modName (P.runProperName name) <$> head foundTypes,
                    showDocs <$> docs
                  ]
            IaClassName name -> do
              let name' = P.runProperName name
                  inferredRes = pursTypeStr name' (Just $ prettyPrintTypeSingleLine iaType) []
                  modName = fromMaybe ofModuleName iaDefinitionModule
              docs <- readDeclarationDocsWithNameType modName TyClassNameType name'
              foundTypes <- getAstDeclarationTypeInModule (Just TyClassNameType) modName name'
              markdownRes (Just $ spanToRange iaSpan) $
                joinMarkup
                  [ Just inferredRes,
                    showTypeSection modName (P.runProperName name) <$> head foundTypes,
                    showDocs <$> docs
                  ]
            IaIdent ident -> do
              markdownRes (Just $ spanToRange iaSpan) $ pursTypeStr ident (Just $ prettyPrintTypeSingleLine iaType) []
            IaBinder binder -> do
              let inferredRes = pursTypeStr (dispayBinderOnHover binder) (Just $ prettyPrintTypeSingleLine iaType) []
              markdownRes (spanToRange <$> binderSourceSpan binder) inferredRes
            IaDecl decl -> do
              markdownRes (Just $ spanToRange iaSpan) $ pursTypeStr (maybe "_" printName $ P.declName decl) (Just $ prettyPrintTypeSingleLine iaType) []
            IaType ty -> do
              markdownRes (Just $ spanToRange iaSpan) $ pursTypeStr (prettyPrintTypeSingleLine ty) (Just $ prettyPrintTypeSingleLine iaType) []
        _ -> nullRes

showTypeSection :: P.ModuleName -> Text -> Text -> Text
showTypeSection mName expr ty = "*" <> P.runModuleName mName <> "*\n" <> pursMd (expr <> " :: " <> ty)

showDocs :: Text -> Text
showDocs d = "**Docs**\n" <> d

joinMarkup :: [Maybe Text] -> Text
joinMarkup = T.intercalate "\n---\n" . catMaybes

-- | Prioritize artifacts that are more likely to be interesting to the developer on hover or click
artifactInterest :: IdeArtifact -> Int
artifactInterest (IdeArtifact {..}) = case iaValue of
  IaBinder {} -> 1
  IaTypeName {} -> 1
  IaClassName {} -> 1
  IaExpr _ _ -> negate (countUnkownsAndVars iaType) -- Prefer expressions with fewer unknowns and type vars
  _ -> 0

countUnkownsAndVars :: P.Type a -> Int
countUnkownsAndVars = P.everythingOnTypes (+) go
  where
    go :: P.Type a -> Int
    go (P.TUnknown _ _) = 1
    go (P.TypeVar _ _) = 1
    go _ = 0

generatedExpr :: P.Expr -> Bool
generatedExpr = \case
  P.Var _ ident -> generatedIdent $ P.disqualify ident
  P.Abs b e -> generatedBinder b || generatedExpr e
  P.App e e' -> generatedExpr e || generatedExpr e'
  P.TypedValue _ e _ -> generatedExpr e
  P.PositionedValue _ _ e -> generatedExpr e
  P.Case es _ -> any generatedExpr es
  _ -> False

generatedBinder :: P.Binder -> Bool
generatedBinder = \case
  P.VarBinder ss ident -> (ss == P.nullSourceSpan) || generatedIdent ident
  P.NamedBinder ss ident _ -> (ss == P.nullSourceSpan) || generatedIdent ident
  _ -> False

generatedIdent :: P.Ident -> Bool
generatedIdent = \case
  P.GenIdent {} -> True
  _ -> False

dispayExprOnHover :: P.Expr -> T.Text
dispayExprOnHover (P.Op _ (P.Qualified _ op)) = P.runOpName op -- Op's hit an infinite loop when pretty printed by themselves
dispayExprOnHover (P.Case _ _) = "<case expr>" -- case expressions are too large to pretty print in hover and are on mulitple lines
dispayExprOnHover expr = ellipsis 128 $ on1Line $ T.strip $ T.pack $ render $ P.prettyPrintValue 8 expr

dispayBinderOnHover :: P.Binder -> T.Text
dispayBinderOnHover binder = ellipsis 32 $ on1Line $ T.strip $ P.prettyPrintBinder binder

on1Line :: T.Text -> T.Text
on1Line = T.intercalate " " . T.lines

ellipsis :: Int -> Text -> Text
ellipsis l t = if T.length t > l then T.take l t <> "..." else t

pursTypeStr :: Text -> Maybe Text -> [P.Comment] -> Text
pursTypeStr word type' comments =
  "```purescript\n"
    <> word
    <> annotation
    <> "\n"
    <> fold (convertComments comments)
    <> "\n```"
  where
    annotation = case type' of
      Just t -> " :: " <> t
      Nothing -> ""

pursMd :: Text -> Text
pursMd t = "```purescript\n" <> t <> "\n```"

data InferError
  = FileNotCached
  | CompilationError P.MultipleErrors
  | InferException Text
  deriving (Show, Exception)

-- inferExprType :: FilePath -> P.Expr -> HandlerM (Either InferError P.SourceType)
-- inferExprType filePath expr = do
--   cacheOpenMb <- cachedRebuild filePath
--   case cacheOpenMb of
--     Nothing -> pure $ Left FileNotCached
--     Just OpenFile {..} -> do
--       inferRes <- runWriterT $ runExceptT $ evalSupplyT 0 $ evalStateT (infer' expr) ((P.emptyCheckState ofStartingEnv) {P.checkCurrentModule = Just ofModuleName})
--       pure $ bimap CompilationError (\(P.TypedValue' _ _ t) -> t) $ fst inferRes

-- inferExprType' :: FilePath -> P.Expr -> HandlerM P.SourceType
-- inferExprType' fp =
--   inferExprType fp >=> \case
--     Right t -> pure t
--     Left e -> throwIO e
