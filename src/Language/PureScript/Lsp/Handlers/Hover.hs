{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Language.PureScript.Lsp.Handlers.Hover where

import Control.Lens ((^.))
import Control.Lens.Getter (to)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.LSP.VFS qualified as VFS
import Language.PureScript qualified as P
import Language.PureScript.CoreFn.Expr qualified as CF
import Language.PureScript.Docs.Convert.Single (convertComments)
import Language.PureScript.Ide.Error (prettyPrintTypeSingleLine)
import Language.PureScript.Lsp.Cache (selectExternModuleNameFromFilePath)
import Language.PureScript.Lsp.Cache.Query (getCoreFnExprAt, getEfDeclarationInModule)
import Language.PureScript.Lsp.Docs (readDeclarationDocsAsMarkdown, readQualifiedNameDocsAsMarkdown)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.Util (efDeclSourceType, getNamesAtPosition, lookupTypeInEnv)
import Language.PureScript.Names (disqualify, runIdent)
import Protolude hiding (to)

hoverHandler :: Server.Handlers HandlerM
hoverHandler =
  Server.requestHandler Message.SMethod_TextDocumentHover $ \req res -> do
    let Types.HoverParams docIdent pos _workDone = req ^. LSP.params
        filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
        docUri =
          docIdent
            ^. LSP.uri
              . to Types.toNormalizedUri
        nullRes = res $ Right $ Types.InR Types.Null

        markdownRes :: Text -> HandlerM ()
        markdownRes md = res $ Right $ Types.InL $ Types.Hover (Types.InL $ Types.MarkupContent Types.MarkupKind_Markdown md) Nothing

        markdownTypeRes :: Text -> Maybe Text -> [P.Comment] -> HandlerM ()
        markdownTypeRes word type' comments =
          markdownRes $ pursTypeStr word type' comments

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

        forLsp :: Maybe a -> (a -> HandlerM ()) -> HandlerM ()
        forLsp val f = maybe nullRes f val

    forLsp filePathMb \filePath -> do
      corefnExprMb <- getCoreFnExprAt filePath pos
      case corefnExprMb of
        Just (CF.Literal _ _) -> nullRes
        Just (CF.Constructor (ss, comments, _meta) tName cMame _) -> do
          docsMb <- do
            mNameMb <- selectExternModuleNameFromFilePath (P.spanName ss)
            maybe (pure Nothing) (`readDeclarationDocsAsMarkdown` P.runProperName tName) mNameMb
          case docsMb of
            Nothing -> markdownTypeRes (P.runProperName cMame) (Just $ P.runProperName tName) comments
            Just docs -> markdownRes docs
        Just (CF.Var (_ss, comments, _meta) (P.Qualified qb ident)) -> do
          case qb of
            P.ByModuleName mName -> do
              docsMb <- readDeclarationDocsAsMarkdown mName (P.runIdent ident)
              case docsMb of
                Just docs -> markdownRes docs
                _ -> do
                  declMb <- getEfDeclarationInModule mName (runIdent ident)
                  markdownTypeRes (P.runIdent ident) (prettyPrintTypeSingleLine . efDeclSourceType <$> declMb) comments
            P.BySourcePos _pos' ->
              markdownTypeRes (P.runIdent ident) Nothing []
        _ -> do
          vfMb <- Server.getVirtualFile docUri
          forLsp vfMb \vf -> do
            mNameMb <- selectExternModuleNameFromFilePath filePath
            forLsp mNameMb \mName -> do
              names <- getNamesAtPosition pos mName (VFS._file_text vf)
              forLsp (head names) \name -> do
                docsMb <- readQualifiedNameDocsAsMarkdown name
                case docsMb of
                  Nothing -> do
                    typeMb <- lookupTypeInEnv filePath name
                    forLsp typeMb \t -> markdownTypeRes (printName $ disqualify name) (Just $ prettyPrintTypeSingleLine t) []
                  Just docs -> markdownRes docs