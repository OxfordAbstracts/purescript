{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.PureScript.LspSimple (main) where

import Control.Lens ((^.))
import Control.Lens.Getter (to)
import Control.Lens.Setter (set)
import Control.Monad.IO.Unlift
import Data.Aeson qualified as A
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types (Diagnostic, Uri)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server (getConfig)
import Language.LSP.Server qualified as Server
import Language.LSP.VFS qualified as VFS
import Language.PureScript qualified as P
import Language.PureScript.CoreFn.Expr qualified as CF
import Language.PureScript.Docs.Convert.Single (convertComments)
import Language.PureScript.Errors (ErrorMessage (ErrorMessage), MultipleErrors (runMultipleErrors), errorCode, errorDocUri, errorSpan, noColorPPEOptions, prettyPrintSingleError)
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Errors.JSON (toSuggestion)
import Language.PureScript.Errors.JSON qualified as JsonErrors
import Language.PureScript.Ide.Error (prettyPrintTypeSingleLine)
import Language.PureScript.Lsp.Cache (selectExternModuleNameFromFilePath, selectExternPathFromModuleName)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationInModule, getAstDeclarationsStartingWith, getCoreFnExprAt, getEfDeclarationInModule)
import Language.PureScript.Lsp.Docs (readDeclarationDocsAsMarkdown, readQualifiedNameDocsAsMarkdown, readQualifiedNameDocsSourceSpan)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.Print (printDeclarationType, printName)
import Language.PureScript.Lsp.Rebuild (rebuildFile)
import Language.PureScript.Lsp.Types (CompleteItemData (CompleteItemData), LspEnvironment, decodeCompleteItemData)
import Language.PureScript.Lsp.Util (declToCompletionItemKind, efDeclSourceSpan, efDeclSourceType, getNamesAtPosition, getWordAt, lookupTypeInEnv, sourcePosToPosition)
import Language.PureScript.Names (disqualify, runIdent)
import Protolude hiding (to)
import Text.PrettyPrint.Boxes (render)

type HandlerM config = ReaderT LspEnvironment ((Server.LspT config IO))

type DiagnosticErrors = IORef (Map Diagnostic ErrorMessage)

insertDiagnosticErrors :: (MonadIO m, Ord k) => IORef (Map k a) -> [a] -> [k] -> m ()
insertDiagnosticErrors diagErrs errs diags = liftIO $ modifyIORef diagErrs (Map.union $ Map.fromList $ zip diags errs)

getDiagnosticErrors :: (MonadIO m, Ord k) => IORef (Map k a) -> [k] -> m (Map k a)
getDiagnosticErrors diagErrs diags = liftIO $ flip Map.restrictKeys (Set.fromList diags) <$> readIORef diagErrs

handlers :: DiagnosticErrors -> Server.Handlers (HandlerM ())
handlers diagErrs =
  mconcat
    [ Server.notificationHandler Message.SMethod_Initialized $ \_not -> sendInfoMsg "Failed to initialise lsp server",
      Server.notificationHandler Message.SMethod_TextDocumentDidOpen $ \msg -> do
        debugLsp "TextDocumentDidOpen"
        let uri :: Uri
            uri = getMsgUri msg
            fileName = Types.uriToFilePath uri

        traverse_ rebuildFile fileName,
      Server.notificationHandler Message.SMethod_TextDocumentDidChange $ \_msg -> debugLsp "TextDocumentDidChange",
      Server.notificationHandler Message.SMethod_TextDocumentDidSave $ \msg -> do
        debugLsp "SMethod_TextDocumentDidSave"
        let uri :: Uri
            uri = getMsgUri msg
            fileName = Types.uriToFilePath uri
        traverse_ rebuildFile fileName,
      Server.notificationHandler Message.SMethod_WorkspaceDidChangeConfiguration $ \_msg -> do
        cfg <- getConfig
        debugLsp $ "Config changed: " <> show cfg,
      Server.notificationHandler Message.SMethod_SetTrace $ \_msg -> debugLsp "SMethod_SetTrace",
      Server.requestHandler Message.SMethod_TextDocumentDiagnostic $ \req res -> do
        debugLsp "SMethod_TextDocumentDiagnostic"
        (errs, diagnostics) <- getFileDiagnotics req
        insertDiagnosticErrors diagErrs errs diagnostics
        res $
          Right $
            Types.DocumentDiagnosticReport $
              Types.InL $
                Types.RelatedFullDocumentDiagnosticReport Types.AString Nothing diagnostics Nothing,
      Server.requestHandler Message.SMethod_TextDocumentCodeAction $ \req res -> do
        let params = req ^. LSP.params
            diags = params ^. LSP.context . LSP.diagnostics
            uri = getMsgUri req

        errs <- Map.toList <$> getDiagnosticErrors diagErrs diags
        res $
          Right $
            Types.InL $
              errs & fmap \(_diag, err) ->
                let textEdits :: [Types.TextEdit]
                    textEdits =
                      toSuggestion err
                        & maybeToList
                          >>= suggestionToEdit

                    suggestionToEdit :: JsonErrors.ErrorSuggestion -> [Types.TextEdit]
                    suggestionToEdit (JsonErrors.ErrorSuggestion replacement (Just JsonErrors.ErrorPosition {..})) =
                      let start = Types.Position (fromIntegral $ startLine - 1) (fromIntegral $ startColumn - 1)
                          end = Types.Position (fromIntegral $ endLine - 1) (fromIntegral $ endColumn - 1)
                       in pure $ Types.TextEdit (Types.Range start end) replacement
                    suggestionToEdit _ = []
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
        debugLsp "SMethod_TextDocumentHover"
        let Types.HoverParams docIdent pos _workDone = req ^. LSP.params
            filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
            docUri =
              docIdent
                ^. LSP.uri
                  . to Types.toNormalizedUri
            nullRes = res $ Right $ Types.InR Types.Null

            markdownRes :: Text -> HandlerM () ()
            markdownRes md = res $ Right $ Types.InL $ Types.Hover (Types.InL $ Types.MarkupContent Types.MarkupKind_Markdown md) Nothing

            markdownTypeRes :: Text -> Maybe Text -> [P.Comment] -> HandlerM () ()
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

            forLsp :: Maybe a -> (a -> HandlerM () ()) -> HandlerM () ()
            forLsp val f = maybe nullRes f val

        forLsp filePathMb \filePath -> do
          corefnExprMb <- getCoreFnExprAt filePath pos
          case corefnExprMb of
            Just (CF.Literal _ _) -> nullRes
            Just (CF.Constructor (ss, comments, _meta) tName cMame _) -> do
              docsMb <- do
                mNameMb <- selectExternModuleNameFromFilePath (P.spanName ss)
                maybe (pure Nothing) (`readDeclarationDocsAsMarkdown` (P.runProperName tName)) mNameMb
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
                        typeMb <- lookupTypeInEnv name
                        forLsp typeMb \t -> markdownTypeRes (printName $ disqualify name) (Just $ prettyPrintTypeSingleLine t) []
                      Just docs -> markdownRes docs,
      Server.requestHandler Message.SMethod_TextDocumentDefinition $ \req res -> do
        sendInfoMsg "SMethod_TextDocumentDefinition"
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

            forLsp :: Maybe a -> (a -> HandlerM () ()) -> HandlerM () ()
            forLsp val f = maybe nullRes f val

        forLsp filePathMb \filePath -> do
          vfMb <- Server.getVirtualFile uri
          forLsp vfMb \vf -> do
            mNameMb <- selectExternModuleNameFromFilePath filePath
            debugLsp $ "Module name: " <> show mNameMb
            debugLsp $ "Pos: " <> show pos
            forLsp mNameMb \mName -> do
              names <- getNamesAtPosition pos mName (VFS._file_text vf)
              debugLsp $ "Found names: " <> show names

              case head names of
                Just name -> do
                  debugLsp $ "Found name: " <> show name
                  spanMb <- readQualifiedNameDocsSourceSpan name
                  debugLsp $ "Found docs span: " <> show spanMb
                  case spanMb of
                    _ -> do
                      case name of
                        P.Qualified (P.BySourcePos pos') _ -> do
                          debugLsp $ "Found source pos: " <> show pos'
                          locationRes filePath (Types.Range (sourcePosToPosition pos') (sourcePosToPosition pos'))
                        P.Qualified (P.ByModuleName nameModule) ident -> do
                          debugLsp $ "Found module name: " <> show nameModule
                          declMb <- getAstDeclarationInModule nameModule (printName ident)
                          debugLsp $ "Found decl: " <> show declMb
                          forLsp declMb \decl -> do
                            modFpMb <- selectExternPathFromModuleName nameModule
                            forLsp modFpMb \modFp -> do
                              debugLsp $ "Found modFp: " <> show modFp
                              let sourceSpan = P.declSourceSpan decl
                              debugLsp $ "Found decl sourceSpan: " <> show sourceSpan
                              locationRes modFp (spanToRange sourceSpan)
                    Just span ->
                      locationRes (P.spanName span) (spanToRange span)
                _ -> do
                  corefnExprMb <- getCoreFnExprAt filePath pos
                  case corefnExprMb of
                    Just (CF.Var (ss, _comments, _meta) (P.Qualified qb ident)) -> do
                      debugLsp $ "Found Corefn Var source span: " <> show ss
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
        debugLsp "SMethod_TextDocumentCompletion"
        let Types.CompletionParams docIdent pos _prog _prog' completionCtx = req ^. LSP.params
            filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
            uri :: Types.NormalizedUri
            uri =
              req
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to Types.toNormalizedUri

            nullRes = res $ Right $ Types.InR $ Types.InR Types.Null

            forLsp :: Maybe a -> (a -> HandlerM () ()) -> HandlerM () ()
            forLsp val f = maybe nullRes f val

        debugLsp $ "Completion params: " <> show completionCtx
        debugLsp $ "filePathMb: " <> show filePathMb
        forLsp filePathMb \filePath -> do
          vfMb <- Server.getVirtualFile uri
          forLsp vfMb \vf -> do
            let word = getWordAt (VFS._file_text vf) pos
            debugLsp $ "Word: " <> show word <> " len " <> show (T.length word)
            if T.length word < 2
              then nullRes
              else do
                mNameMb <- selectExternModuleNameFromFilePath filePath
                debugLsp $ "Module name: " <> show mNameMb
                forLsp mNameMb \mName -> do
                  decls <- getAstDeclarationsStartingWith mName word
                  debugLsp $ "Found decls: " <> show decls
                  res $
                    Right $
                      Types.InL $
                        decls <&> \(declModule, decl) ->
                          let label = foldMap printName (P.declName decl)
                           in Types.CompletionItem
                                { _label = label,
                                  _labelDetails =
                                    Just $
                                      Types.CompletionItemLabelDetails
                                        (Just $ " " <> printDeclarationType decl)
                                        (Just $ " " <> P.runModuleName declModule),
                                  _kind = declToCompletionItemKind decl,
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
                                  _data_ = Just $ A.toJSON $ Just $ CompleteItemData filePath mName declModule decl
                                },
      Server.requestHandler Message.SMethod_CompletionItemResolve $ \req res -> do
        debugLsp "SMethod_CompletionItemResolve"
        let completionItem = req ^. LSP.params
            result = completionItem ^. LSP.data_ & decodeCompleteItemData

        case result of
          A.Success (Just (CompleteItemData _filePath _mName declModule decl)) -> do
            let label = foldMap printName (P.declName decl)
            docsMb <- readDeclarationDocsAsMarkdown declModule label
            let addDocs :: Types.CompletionItem -> Types.CompletionItem
                addDocs =
                  docsMb & maybe
                    identity
                    \docs ->
                      set LSP.documentation (Just $ Types.InR $ Types.MarkupContent Types.MarkupKind_Markdown docs)

                addImport :: Types.CompletionItem -> Types.CompletionItem
                addImport = identity
            res $
              Right $
                completionItem
                  & addDocs
                  & addImport
          _ -> res $ Right completionItem
    ]
  where
    getFileDiagnotics :: (LSP.HasUri a2 Uri, LSP.HasTextDocument a1 a2, LSP.HasParams s a1) => s -> HandlerM config ([ErrorMessage], [Types.Diagnostic])
    getFileDiagnotics msg = do
      let uri :: Uri
          uri = getMsgUri msg
          fileName = Types.uriToFilePath uri
      case fileName of
        Just file -> do
          res <- rebuildFile file
          getResultDiagnostics res
        Nothing -> do
          sendInfoMsg $ "No file path for uri: " <> show uri
          pure ([], [])

    getMsgUri :: (LSP.HasParams s a1, LSP.HasTextDocument a1 a2, LSP.HasUri a2 a3) => s -> a3
    getMsgUri msg = msg ^. LSP.params . LSP.textDocument . LSP.uri

    getResultDiagnostics :: Either ([(FilePath, Text)], P.MultipleErrors) (FilePath, P.MultipleErrors) -> HandlerM config ([ErrorMessage], [Types.Diagnostic])
    getResultDiagnostics res = case res of
      Left (_, errs) -> do
        let errors = runMultipleErrors errs
            diags = errorMessageDiagnostic Types.DiagnosticSeverity_Error <$> errors
        pure (errors, diags)
      Right (_, errs) | Errors.nonEmpty errs -> do
        let errors = runMultipleErrors errs
            diags = errorMessageDiagnostic Types.DiagnosticSeverity_Warning <$> errors
        pure (errors, diags)
      _ -> pure ([], [])
      where
        errorMessageDiagnostic :: Types.DiagnosticSeverity -> ErrorMessage -> Types.Diagnostic
        errorMessageDiagnostic severity msg@((ErrorMessage _hints _)) =
          Types.Diagnostic
            (Types.Range start end)
            (Just severity)
            (Just $ Types.InR $ errorCode msg)
            (Just $ Types.CodeDescription $ Types.Uri $ errorDocUri msg)
            (T.pack <$> spanName)
            (T.pack $ render $ prettyPrintSingleError noColorPPEOptions msg)
            Nothing
            Nothing
            Nothing
          where
            notFound = Types.Position 0 0
            (spanName, start, end) = getPositions $ errorSpan msg

            getPositions = fromMaybe (Nothing, notFound, notFound) . getPositionsMb

            getPositionsMb = fmap $ \spans ->
              let (Errors.SourceSpan name (Errors.SourcePos startLine startCol) (Errors.SourcePos endLine endCol)) =
                    NEL.head spans
               in ( Just name,
                    Types.Position (fromIntegral $ startLine - 1) (fromIntegral $ startCol - 1),
                    Types.Position (fromIntegral $ endLine - 1) (fromIntegral $ endCol - 1)
                  )

spanToRange :: Errors.SourceSpan -> Types.Range
spanToRange (Errors.SourceSpan _ start end) =
  Types.Range
    (sourcePosToPosition start)
    (sourcePosToPosition end)

sendInfoMsg :: (Server.MonadLsp config f) => Text -> f ()
sendInfoMsg msg = Server.sendNotification Message.SMethod_WindowShowMessage (Types.ShowMessageParams Types.MessageType_Info msg)

main :: LspEnvironment -> IO Int
main lspEnv = do
  diagErrs <- newIORef Map.empty
  Server.runServer $
    Server.ServerDefinition
      { parseConfig = const $ const $ Right (),
        onConfigChange = const $ pure (),
        defaultConfig = (),
        configSection = "oa-purescript-lsp",
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = \_caps -> handlers diagErrs,
        interpretHandler = \env ->
          Server.Iso
            ( Server.runLspT env . flip runReaderT lspEnv
            )
            liftIO,
        options = lspOptions
      }

syncOptions :: Types.TextDocumentSyncOptions
syncOptions =
  Types.TextDocumentSyncOptions
    { Types._openClose = Just True,
      Types._change = Just Types.TextDocumentSyncKind_Incremental,
      Types._willSave = Just False,
      Types._willSaveWaitUntil = Just False,
      Types._save = Just $ Types.InR $ Types.SaveOptions $ Just False
    }

lspOptions :: Server.Options
lspOptions =
  Server.defaultOptions
    { Server.optTextDocumentSync = Just syncOptions,
      Server.optExecuteCommandCommands = Just ["lsp-purescript-command"]
    }
