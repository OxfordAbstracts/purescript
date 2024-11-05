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
import Language.PureScript.Docs.Types qualified as Docs
import Language.PureScript.Ide.Error (prettyPrintTypeSingleLine)
import Language.PureScript.Lsp.AtPosition (binderSourceSpan, getImportRefNameType, spanToRange)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationTypeInModule)
import Language.PureScript.Lsp.Docs (readDeclarationDocsWithNameType, readModuleDocs)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.State (cachedFilePaths, cachedRebuild)
import Language.PureScript.Lsp.Types (OpenFile (..))
import Language.PureScript.Lsp.Util (positionToSourcePos)
import Language.PureScript.TypeChecker.IdeArtifacts (IdeArtifact (..), IdeArtifactValue (..), getArtifactsAtPosition, smallestArtifact, useSynonymns)
import Protolude hiding (handle, to)

hoverHandler :: Server.Handlers HandlerM
hoverHandler = Server.requestHandler Message.SMethod_TextDocumentHover $ \req res -> do
  let Types.HoverParams docIdent startPos _prog = req ^. LSP.params
      filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri

      nullRes = res $ Right $ Types.InR Types.Null

      markdownRes range md =
        res $ Right $ Types.InL $ Types.Hover (Types.InL $ Types.MarkupContent Types.MarkupKind_Markdown md) range

      forLsp :: Maybe a -> (a -> HandlerM ()) -> HandlerM ()
      forLsp val f = maybe nullRes f val

      lookupExprTypes :: Maybe Text -> Maybe P.ModuleName -> Maybe LspNameType -> HandlerM [Text]
      lookupExprTypes (Just ident) (Just modName) nameType =
        fmap (showTypeSection modName ident) <$> getAstDeclarationTypeInModule nameType modName ident
      lookupExprTypes _ _ _ = pure []

      lookupExprDocs :: Maybe Text -> Maybe P.ModuleName -> Maybe LspNameType -> HandlerM (Maybe Text)
      lookupExprDocs (Just ident) (Just modName) (Just nameType) =
        readDeclarationDocsWithNameType modName nameType ident
      lookupExprDocs _ _ _ = pure Nothing

  forLsp filePathMb \filePath -> do
    cacheOpenMb <- cachedRebuild filePath
    debugLsp $ "Cache found: " <> show (isJust cacheOpenMb)
    when (isNothing cacheOpenMb) do
      debugLsp $ "file path not cached: " <> T.pack filePath
      debugLsp . show =<< cachedFilePaths
    forLsp cacheOpenMb \OpenFile {..} -> do
      let allArtifacts = P.checkIdeArtifacts ofEndCheckState
          atPos = getArtifactsAtPosition (positionToSourcePos startPos) allArtifacts
      debugLsp $ "hover artiacts length: " <> show (length atPos)
      case smallestArtifact (\a -> (artifactInterest a, negate $ countUnkownsAndVars $ iaType a)) atPos of
        Just (IdeArtifact {..}) ->
          case iaValue of
            IaExpr exprTxt ident nameType -> do
              let inferredRes =
                    pursTypeStr
                      exprTxt
                      ( Just $
                          prettyPrintTypeSingleLine $
                            useSynonymns allArtifacts iaType
                      )
                      []
              foundTypes <- lookupExprTypes ident iaDefinitionModule nameType
              docs <- lookupExprDocs ident iaDefinitionModule nameType
              markdownRes (Just $ spanToRange iaSpan) $
                joinMarkup
                  [ Just inferredRes,
                    showDocs <$> docs,
                    head foundTypes
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
                    showDocs <$> docs,
                    showTypeSection modName (P.runProperName name) <$> head foundTypes
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
              markdownRes (Just $ spanToRange iaSpan) $ pursTypeStr ident (Just $ prettyPrintTypeSingleLine $ useSynonymns allArtifacts iaType) []
            IaBinder binder -> do
              let inferredRes = pursTypeStr (dispayBinderOnHover binder) (Just $ prettyPrintTypeSingleLine $ useSynonymns allArtifacts iaType) []
              markdownRes (spanToRange <$> binderSourceSpan binder) inferredRes
            IaDecl decl _ -> do
              markdownRes (Just $ spanToRange iaSpan) $ pursTypeStr (fromMaybe "_" decl) (Just $ prettyPrintTypeSingleLine $ useSynonymns allArtifacts iaType) []
            IaType ty -> do
              markdownRes (Just $ spanToRange iaSpan) $ pursTypeStr (prettyPrintTypeSingleLine ty) (Just $ prettyPrintTypeSingleLine iaType) []
            IaModule modName -> do
              docsMb <- readModuleDocs modName
              case docsMb of
                Just docs | Just comments <- Docs.modComments docs -> markdownRes (Just $ spanToRange iaSpan) comments
                _ -> nullRes
            IaImport modName ref -> do
              let name = P.declRefName ref
                  nameType = getImportRefNameType ref
                  name' = printName name
              docs <- readDeclarationDocsWithNameType modName nameType name'
              foundTypes <- getAstDeclarationTypeInModule (Just nameType) modName name'
              markdownRes (Just $ spanToRange iaSpan) $
                joinMarkup
                  [ showDocs <$> docs,
                    showTypeSection modName name' <$> head foundTypes
                  ]
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
  IaBinder {} -> 2
  IaTypeName {} -> 3
  IaClassName {} -> 3
  IaExpr _ _ _ -> negate (countUnkownsAndVars iaType)
  _ -> 1

countUnkownsAndVars :: P.Type a -> Int
countUnkownsAndVars = P.everythingOnTypes (+) go
  where
    go :: P.Type a -> Int
    go (P.TUnknown _ _) = 1
    go (P.TypeVar _ _) = 1
    go _ = 0

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
