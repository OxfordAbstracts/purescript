{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Language.PureScript.Lsp.Handlers where

import Control.Lens ((^.))
import Control.Lens.Getter (to)
import Control.Lens.Setter (set)
import Data.Aeson qualified as A
import Data.Map qualified as Map
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.LSP.VFS qualified as VFS
import Language.PureScript qualified as P
import Language.PureScript.Compile (compile)
import Language.PureScript.CoreFn.Expr qualified as CF
import Language.PureScript.DB (dbFile)
import Language.PureScript.Docs.Convert.Single (convertComments)
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Ide.Error (prettyPrintTypeSingleLine)
import Language.PureScript.Ide.Imports (Import (..))
import Language.PureScript.Lsp.Cache (selectExternModuleNameFromFilePath, selectExternPathFromModuleName, updateAvailableSrcs)
import Language.PureScript.Lsp.Cache.Query (CompletionResult (crModule, crName, crType), getAstDeclarationInModule, getAstDeclarationsStartingWith, getAstDeclarationsStartingWithAndSearchingModuleNames, getAstDeclarationsStartingWithOnlyInModule, getCoreFnExprAt, getEfDeclarationInModule)
import Language.PureScript.Lsp.Diagnostics (errorMessageDiagnostic, getFileDiagnotics, getMsgUri)
import Language.PureScript.Lsp.Docs (readDeclarationDocsAsMarkdown, readQualifiedNameDocsAsMarkdown, readQualifiedNameDocsSourceSpan)
import Language.PureScript.Lsp.Imports (addImportToTextEdit, getIdentModuleQualifier, getMatchingImport)
import Language.PureScript.Lsp.Log (logPerfStandard)
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.Rebuild (codegenTargets, rebuildFile)
import Language.PureScript.Lsp.ServerConfig (ServerConfig, getMaxCompletions, setTraceValue)
import Language.PureScript.Lsp.State (cancelRequest, removedCachedRebuild)
import Language.PureScript.Lsp.Types (CompleteItemData (CompleteItemData), LspConfig (confOutputPath), LspEnvironment (lspConfig, lspDbConnection), decodeCompleteItemData)
import Language.PureScript.Lsp.Util (efDeclSourceSpan, efDeclSourceType, getNamesAtPosition, getWordAt, lookupTypeInEnv, sourcePosToPosition)
import Language.PureScript.Make.Index (initDb)
import Language.PureScript.Names (disqualify, runIdent)
import Protolude hiding (to)
import System.Directory (createDirectoryIfMissing, listDirectory, removePathForcibly)
import System.FilePath ((</>))
import System.IO.UTF8 (readUTF8FilesT)

type HandlerM config = ReaderT LspEnvironment (Server.LspT config IO)

handlers :: Server.Handlers (HandlerM ServerConfig)
handlers =
  mconcat
    [ Server.notificationHandler Message.SMethod_Initialized $ \_not -> do
        void updateAvailableSrcs
        sendInfoMsg "Lsp initialized",
      Server.notificationHandler Message.SMethod_TextDocumentDidOpen $ \_msg -> do
        pure (),
      Server.notificationHandler Message.SMethod_TextDocumentDidSave $ \_msg -> do
        pure (),
      Server.notificationHandler Message.SMethod_TextDocumentDidClose $ \msg -> do
        let uri :: Uri
            uri = getMsgUri msg
            fileName = Types.uriToFilePath uri
        traverse_ removedCachedRebuild fileName,
      Server.notificationHandler Message.SMethod_WorkspaceDidChangeConfiguration $ \_msg -> do
        pure (),
      Server.notificationHandler Message.SMethod_SetTrace $ \msg -> do
        setTraceValue $ msg ^. LSP.params . LSP.value, -- probably no need to do this
      Server.notificationHandler Message.SMethod_CancelRequest $ \msg -> do
        let reqId = msg ^. LSP.params . LSP.id
        cancelRequest reqId,
      Server.requestHandler Message.SMethod_TextDocumentDiagnostic $ \req res -> do
        (_errs, diagnostics) <- getFileDiagnotics req
        res $
          Right $
            Types.DocumentDiagnosticReport $
              Types.InL $
                Types.RelatedFullDocumentDiagnosticReport Types.AString Nothing diagnostics Nothing,
      Server.requestHandler Message.SMethod_TextDocumentCodeAction $ \req res -> do
        let params = req ^. LSP.params
            diags = params ^. LSP.context . LSP.diagnostics
            uri = getMsgUri req

        res $
          Right $
            Types.InL $
              diags <&> \diag ->
                let textEdits = case A.fromJSON <$> diag ^. LSP.data_ of
                      Just (A.Success tes) -> tes
                      _ -> []
                 in Types.InR $
                      Types.CodeAction
                        "Apply suggestion"
                        (Just Types.CodeActionKind_QuickFix)
                        (Just diags)
                        (Just True)
                        Nothing -- disabled
                        ( Just $
                            Types.WorkspaceEdit
                              (Just $ Map.singleton uri textEdits)
                              Nothing
                              Nothing
                        )
                        Nothing
                        Nothing,
      Server.requestHandler Message.SMethod_TextDocumentHover $ \req res -> do
        let Types.HoverParams docIdent pos _workDone = req ^. LSP.params
            filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
            docUri =
              docIdent
                ^. LSP.uri
                  . to Types.toNormalizedUri
            nullRes = res $ Right $ Types.InR Types.Null

            markdownRes :: Text -> HandlerM ServerConfig ()
            markdownRes md = res $ Right $ Types.InL $ Types.Hover (Types.InL $ Types.MarkupContent Types.MarkupKind_Markdown md) Nothing

            markdownTypeRes :: Text -> Maybe Text -> [P.Comment] -> HandlerM ServerConfig ()
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

            forLsp :: Maybe a -> (a -> HandlerM ServerConfig ()) -> HandlerM ServerConfig ()
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
                      Just docs -> markdownRes docs,
      Server.requestHandler Message.SMethod_TextDocumentDefinition $ \req res -> do
        let Types.DefinitionParams docIdent pos _prog _prog' = req ^. LSP.params
            filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
            uri :: Types.NormalizedUri
            uri =
              req
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to Types.toNormalizedUri

            nullRes = res $ Right $ Types.InR $ Types.InR Types.Null

            locationRes fp range = res $ Right $ Types.InL $ Types.Definition $ Types.InL $ Types.Location (Types.filePathToUri fp) range

            forLsp :: Maybe a -> (a -> HandlerM ServerConfig ()) -> HandlerM ServerConfig ()
            forLsp val f = maybe nullRes f val
        forLsp filePathMb \filePath -> do
          vfMb <- Server.getVirtualFile uri
          forLsp vfMb \vf -> do
            mNameMb <- selectExternModuleNameFromFilePath filePath
            forLsp mNameMb \mName -> do
              names <- getNamesAtPosition pos mName (VFS._file_text vf)

              case head names of
                Just name -> do
                  spanMb <- readQualifiedNameDocsSourceSpan name
                  case spanMb of
                    _ -> do
                      case name of
                        P.Qualified (P.BySourcePos pos') _ -> do
                          locationRes filePath (Types.Range (sourcePosToPosition pos') (sourcePosToPosition pos'))
                        P.Qualified (P.ByModuleName nameModule) ident -> do
                          declMb <- getAstDeclarationInModule nameModule (printName ident)
                          forLsp declMb \decl -> do
                            modFpMb <- selectExternPathFromModuleName nameModule
                            forLsp modFpMb \modFp -> do
                              let sourceSpan = P.declSourceSpan decl
                              locationRes modFp (spanToRange sourceSpan)
                    Just span ->
                      locationRes (P.spanName span) (spanToRange span)
                _ -> do
                  corefnExprMb <- getCoreFnExprAt filePath pos
                  case corefnExprMb of
                    Just (CF.Var (_ss, _comments, _meta) (P.Qualified qb ident)) -> do
                      let name = P.runIdent ident
                      case qb of
                        P.ByModuleName coreMName -> do
                          declMb <- getEfDeclarationInModule coreMName name
                          forLsp declMb \decl -> do
                            modFpMb <- selectExternPathFromModuleName coreMName
                            forLsp modFpMb \modFp -> do
                              let sourceSpan = efDeclSourceSpan decl
                              locationRes modFp (spanToRange sourceSpan)
                        P.BySourcePos srcPos ->
                          locationRes filePath (Types.Range (sourcePosToPosition srcPos) (sourcePosToPosition srcPos))
                    _ -> nullRes,
      Server.requestHandler Message.SMethod_TextDocumentCompletion $ \req res -> do
        let Types.CompletionParams docIdent pos _prog _prog' _completionCtx = req ^. LSP.params
            filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
            uri :: Types.NormalizedUri
            uri =
              req
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to Types.toNormalizedUri

            nullRes = res $ Right $ Types.InR $ Types.InR Types.Null

            forLsp :: Maybe a -> (a -> HandlerM ServerConfig ()) -> HandlerM ServerConfig ()
            forLsp val f = maybe nullRes f val

        forLsp filePathMb \filePath -> do
          vfMb <- Server.getVirtualFile uri
          forLsp vfMb \vf -> do
            let (range, word) = getWordAt (VFS._file_text vf) pos
            if T.length word < 2
              then nullRes
              else do
                mNameMb <- selectExternModuleNameFromFilePath filePath
                forLsp mNameMb \mName -> do
                  let withQualifier = getIdentModuleQualifier word
                      wordWithoutQual = maybe word snd withQualifier
                  limit <- getMaxCompletions
                  matchingImport <- maybe (pure Nothing) (getMatchingImport filePath . fst) withQualifier
                  -- matchingImport =
                  decls <- case (matchingImport, withQualifier) of
                    (Just (Import importModuleName _ _), _) -> getAstDeclarationsStartingWithOnlyInModule importModuleName wordWithoutQual
                    (_, Just (wordModuleName, _)) -> getAstDeclarationsStartingWithAndSearchingModuleNames mName wordModuleName wordWithoutQual
                    _ -> logPerfStandard "getAstDeclarationsStartingWith" $ getAstDeclarationsStartingWith mName wordWithoutQual
                  -- Just
                  res $
                    Right $
                      Types.InR $
                        Types.InL $
                          Types.CompletionList (length decls >= limit) Nothing $
                            decls <&> \cr ->
                              let label = crName cr
                               in Types.CompletionItem
                                    { _label = label,
                                      _labelDetails =
                                        Just $
                                          Types.CompletionItemLabelDetails
                                            (Just $ " " <> crType cr)
                                            (Just $ P.runModuleName (crModule cr)),
                                      _kind = Nothing, --  Maybe Types.CompletionItemKind TODO: add kind
                                      _tags = Nothing,
                                      _detail = Nothing,
                                      _documentation = Nothing,
                                      _deprecated = Nothing, --  Maybe Bool
                                      _preselect = Nothing, --  Maybe Bool
                                      _sortText = Nothing, --  Maybe Text
                                      _filterText = Nothing, --  Maybe Text
                                      _insertText = Nothing, --  Maybe Text
                                      _insertTextFormat = Nothing, --  Maybe Types.InsertTextFormat
                                      _insertTextMode = Nothing, --  Maybe Types.InsertTextMode
                                      _textEdit = Nothing, --  Maybe
                                      --                (Types.TextEdit Types.|? Types.InsertReplaceEdit)
                                      _textEditText = Nothing, --  Maybe Text
                                      _additionalTextEdits = Nothing, --  Maybe [Types.TextEdit]
                                      _commitCharacters = Nothing, --  Maybe [Text]
                                      _command = Nothing, --  Maybe Types.Command
                                      _data_ = Just $ A.toJSON $ Just $ CompleteItemData filePath mName (crModule cr) label word range
                                    },
      Server.requestHandler Message.SMethod_CompletionItemResolve $ \req res -> do
        let completionItem = req ^. LSP.params
            result = completionItem ^. LSP.data_ & decodeCompleteItemData

        case result of
          A.Success (Just cid@(CompleteItemData _filePath _mName declModule label _ _)) -> do
            docsMb <- readDeclarationDocsAsMarkdown declModule label
            withImports <- addImportToTextEdit completionItem cid
            let addDocs :: Types.CompletionItem -> Types.CompletionItem
                addDocs =
                  docsMb & maybe
                    identity
                    \docs ->
                      set LSP.documentation (Just $ Types.InR $ Types.MarkupContent Types.MarkupKind_Markdown docs)
            res $
              Right $
                withImports
                  & addDocs
          _ -> res $ Right completionItem,
      Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"delete output") $ \_req res -> do
        outDir <- asks (confOutputPath . lspConfig)
        liftIO $ createDirectoryIfMissing True outDir
        contents <- liftIO $ listDirectory outDir
        for_ contents \f -> do
          unless (f == dbFile || dbFile `isPrefixOf` f) do
            let path = outDir </> f
            liftIO $ removePathForcibly path
        res $ Right A.Null,
      Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"build") $ \_req res -> do
        config <- asks lspConfig
        conn <- asks lspDbConnection
        liftIO $ initDb conn
        input <- updateAvailableSrcs
        moduleFiles <- liftIO $ readUTF8FilesT input
        (result, warnings) <-
          liftIO $
            compile
              (P.Options False False codegenTargets)
              moduleFiles
              conn
              (confOutputPath config)
              False
        let diags :: [Types.Diagnostic]
            diags =
              (errorMessageDiagnostic Types.DiagnosticSeverity_Error <$> either P.runMultipleErrors (const []) result)
                <> (errorMessageDiagnostic Types.DiagnosticSeverity_Warning <$> P.runMultipleErrors warnings)
        res $ Right $ A.toJSON diags
    ]

spanToRange :: Errors.SourceSpan -> Types.Range
spanToRange (Errors.SourceSpan _ start end) =
  Types.Range
    (sourcePosToPosition start)
    (sourcePosToPosition end)

sendInfoMsg :: (Server.MonadLsp config f) => Text -> f ()
sendInfoMsg msg = Server.sendNotification Message.SMethod_WindowShowMessage (Types.ShowMessageParams Types.MessageType_Info msg)
