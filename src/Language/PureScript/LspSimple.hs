{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.PureScript.LspSimple (main) where

import Control.Lens (Field1 (_1), view, (^.))
import Control.Lens.Getter (to)
import Control.Lens.Setter (set)
import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.IO.Unlift
import Control.Monad.Reader (mapReaderT)
import Data.Aeson qualified as A
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH (listT)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types (Diagnostic, Uri)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server (getConfig)
import Language.LSP.Server qualified as Server
import Language.LSP.VFS qualified as VFS
import Language.PureScript qualified as P
import Language.PureScript.AST.SourcePos (SourcePos (sourcePosColumn), nullSourceSpan)
import Language.PureScript.AST.SourcePos qualified as P
import Language.PureScript.Constants.TH (ty)
import Language.PureScript.CoreFn.Expr (extractAnn)
import Language.PureScript.CoreFn.Expr qualified as CF
import Language.PureScript.Docs.Convert.Single (convertComments)
import Language.PureScript.Errors (ErrorMessage (ErrorMessage), MultipleErrors (runMultipleErrors), errorCode, errorDocUri, errorSpan, noColorPPEOptions, prettyPrintSingleError)
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Errors.JSON (toSuggestion)
import Language.PureScript.Errors.JSON qualified as JsonErrors
import Language.PureScript.Ide.Error (IdeError (GeneralError, RebuildError), prettyPrintTypeSingleLine, textError)
import Language.PureScript.Ide.Logging (runErrLogger)
import Language.PureScript.Ide.Types (Completion (Completion, complDocumentation, complExpandedType, complType), IdeLogLevel (LogAll), declarationType, _IdeDeclModule)
import Language.PureScript.Lsp.Cache (selectExternModuleNameFromFilePath, selectExternPathFromModuleName)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationInModule, getAstDeclarationsStartingWith, getCoreFnExprAt, getEfDeclarationInModule, getEfDeclarationsAtSrcPos)
import Language.PureScript.Lsp.Docs (readDeclarationDocsAsMarkdown, readQualifiedNameDocsAsMarkdown, readQualifiedNameDocsSourceSpan)
import Language.PureScript.Lsp.Print (printDeclarationType, printDeclarationTypeMb, printName)
import Language.PureScript.Lsp.Rebuild (rebuildFile)
import Language.PureScript.Lsp.State (initFinished, waitForInit)
import Language.PureScript.Lsp.Types (LspEnvironment)
import Language.PureScript.Lsp.Util (declToCompletionItemKind, efDeclComments, efDeclSourceSpan, efDeclSourceType, getNamesAtPosition, getWordAt, lookupTypeInEnv, sourcePosToPosition)
import Language.PureScript.Names (disqualify, runIdent)
import Language.PureScript.Names qualified as P
import Protolude hiding (to)
import System.Directory (createDirectoryIfMissing)
import Text.PrettyPrint.Boxes (render)
import "monad-logger" Control.Monad.Logger (LoggingT, logDebug, logDebugN, logErrorN, logWarnN, mapLoggingT)

type HandlerM config = Server.LspT config (ReaderT LspEnvironment (LoggingT IO))

type LspM = ReaderT LspEnvironment (LoggingT (ExceptT IdeError IO))

liftLsp :: LspM a -> HandlerM config a
liftLsp = lift . mapReaderT (mapLoggingT (throwIdeError <=< runExceptT))
  where
    throwIdeError = \case
      Left err -> liftIO $ throwIO err
      Right a -> pure a

liftLspWithErr :: LspM a -> HandlerM config (Either IdeError a)
liftLspWithErr = lift . flip catchError errorHandler . mapReaderT (mapLoggingT runExceptT)
  where
    errorHandler ::
      IOException ->
      ReaderT LspEnvironment (LoggingT IO) (Either IdeError a)
    errorHandler err = do
      logErrorN $ T.pack (show err)
      pure $ Left $ GeneralError $ show err

type DiagnosticErrors = IORef (Map Diagnostic ErrorMessage)

insertDiagnosticError :: (MonadIO m, Ord k) => IORef (Map k a) -> a -> k -> m ()
insertDiagnosticError diagErrs err diag = liftIO $ modifyIORef diagErrs (Map.insert diag err)

insertDiagnosticErrors :: (MonadIO m, Ord k) => IORef (Map k a) -> [a] -> [k] -> m ()
insertDiagnosticErrors diagErrs errs diags = liftIO $ modifyIORef diagErrs (Map.union $ Map.fromList $ zip diags errs)

getDiagnosticError :: (MonadIO m, Ord k) => IORef (Map k a) -> k -> m (Maybe a)
getDiagnosticError diagErrs diags = liftIO $ Map.lookup diags <$> readIORef diagErrs

getDiagnosticErrors :: (MonadIO m, Ord k) => IORef (Map k a) -> [k] -> m (Map k a)
getDiagnosticErrors diagErrs diags = liftIO $ flip Map.restrictKeys (Set.fromList diags) <$> readIORef diagErrs

handlers :: DiagnosticErrors -> Server.Handlers (HandlerM ())
handlers diagErrs =
  mconcat
    [ Server.notificationHandler Message.SMethod_Initialized $ \_not -> do
        res <- liftLspWithErr do
          initFinished
          logDebugN "Init finished"
        case res of
          Left err -> do
            liftLsp $ logErrorN $ "Initalise error: " <> show err
            sendInfoMsg "Failed to initialise lsp server"
          Right _ -> sendInfoMsg "OA purs lsp server initialized",
      Server.notificationHandler Message.SMethod_TextDocumentDidOpen $ \msg -> do
        liftLsp $ logDebugN "TextDocumentDidOpen"
        let uri :: Uri
            uri = getMsgUri msg
            fileName = Types.uriToFilePath uri
        void $ liftLspWithErr do
          waitForInit
          traverse rebuildFile fileName,
      Server.notificationHandler Message.SMethod_TextDocumentDidChange $ \msg -> do
        liftLsp $ logDebugN "TextDocumentDidChange",
      Server.notificationHandler Message.SMethod_TextDocumentDidSave $ \msg -> do
        liftLsp $ logDebugN "SMethod_TextDocumentDidSave"
        let uri :: Uri
            uri = getMsgUri msg
            fileName = Types.uriToFilePath uri
        void $ liftLspWithErr do
          waitForInit
          traverse rebuildFile fileName,
      Server.notificationHandler Message.SMethod_WorkspaceDidChangeConfiguration $ \msg -> do
        cfg <- getConfig
        liftLsp $ logDebugN $ "Config changed: " <> show cfg,
      Server.notificationHandler Message.SMethod_SetTrace $ \msg -> do
        liftLsp $ logDebugN "SMethod_SetTrace",
      Server.requestHandler Message.SMethod_TextDocumentDiagnostic $ \req res -> do
        liftLsp $ logDebugN "SMethod_TextDocumentDiagnostic"
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
              errs & fmap \(diag, err) ->
                let textEdits :: [Types.TextEdit]
                    textEdits =
                      toSuggestion err
                        & maybeToList
                          >>= suggestionToEdit

                    suggestionToEdit :: JsonErrors.ErrorSuggestion -> [Types.TextEdit]
                    suggestionToEdit (JsonErrors.ErrorSuggestion replacement (Just errorPos@JsonErrors.ErrorPosition {..})) =
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
        liftLsp $ logDebugN "SMethod_TextDocumentHover"
        let Types.HoverParams docIdent pos@(Types.Position line col) _workDone = req ^. LSP.params
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
          corefnExprMb <- liftLsp $ getCoreFnExprAt filePath pos
          case corefnExprMb of
            Just (CF.Literal _ _) -> nullRes
            Just (CF.Constructor (ss, comments, meta) tName cMame _) -> do
              docsMb <- liftLsp do
                mNameMb <- selectExternModuleNameFromFilePath (P.spanName ss)
                maybe (pure Nothing) (flip readDeclarationDocsAsMarkdown (P.runProperName tName)) mNameMb
              case docsMb of
                Nothing -> markdownTypeRes (P.runProperName cMame) (Just $ P.runProperName tName) comments
                Just docs -> markdownRes docs
            Just (CF.Var (_ss, comments, meta) (P.Qualified qb ident)) -> do
              case qb of
                P.ByModuleName mName -> do
                  docsMb <- liftLsp $ readDeclarationDocsAsMarkdown mName (P.runIdent ident)
                  case docsMb of
                    Just docs -> markdownRes docs
                    _ -> do
                      declMb <- liftLsp $ getEfDeclarationInModule mName (runIdent ident)
                      markdownTypeRes (P.runIdent ident) (prettyPrintTypeSingleLine . efDeclSourceType <$> declMb) comments
                P.BySourcePos pos' ->
                  markdownTypeRes (P.runIdent ident) Nothing []
            _ -> do
              vfMb <- Server.getVirtualFile docUri
              forLsp vfMb \vf -> do
                mNameMb <- liftLsp $ selectExternModuleNameFromFilePath filePath
                forLsp mNameMb \mName -> do
                  names <- liftLsp $ getNamesAtPosition pos mName (VFS._file_text vf)
                  forLsp (head names) \name -> do
                    docsMb <- liftLsp $ readQualifiedNameDocsAsMarkdown name
                    case docsMb of
                      Nothing -> do
                        typeMb <- liftLsp $ lookupTypeInEnv name
                        forLsp typeMb \t -> do
                          markdownTypeRes (printName $ disqualify name) (Just $ prettyPrintTypeSingleLine t) []
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
            mNameMb <- liftLsp $ selectExternModuleNameFromFilePath filePath
            liftLsp $ logDebugN $ "Module name: " <> show mNameMb
            liftLsp $ logDebugN $ "Pos: " <> show pos
            forLsp mNameMb \mName -> do
              names <- liftLsp $ getNamesAtPosition pos mName (VFS._file_text vf)
              liftLsp $ logDebugN $ "Found names: " <> show names

              case head names of
                Just name -> do
                  liftLsp $ logDebugN $ "Found name: " <> show name
                  spanMb <- liftLsp $ readQualifiedNameDocsSourceSpan name
                  liftLsp $ logDebugN $ "Found docs span: " <> show spanMb
                  case spanMb of
                    _ -> do
                      case name of
                        P.Qualified (P.BySourcePos pos') _ -> do
                          liftLsp $ logDebugN $ "Found source pos: " <> show pos'
                          locationRes filePath (Types.Range (sourcePosToPosition pos') (sourcePosToPosition pos'))
                        P.Qualified (P.ByModuleName nameModule) ident -> do
                          liftLsp $ logDebugN $ "Found module name: " <> show nameModule
                          declMb <- liftLsp $ getAstDeclarationInModule nameModule (printName ident)
                          liftLsp $ logDebugN $ "Found decl: " <> show declMb
                          forLsp declMb \decl -> do
                            modFpMb <- liftLsp $ selectExternPathFromModuleName nameModule
                            forLsp modFpMb \modFp -> do
                              liftLsp $ logDebugN $ "Found modFp: " <> show modFp
                              let sourceSpan = P.declSourceSpan decl
                              liftLsp $ logDebugN $ "Found decl sourceSpan: " <> show sourceSpan
                              locationRes modFp (spanToRange sourceSpan)
                    Just span ->
                      locationRes (P.spanName span) (spanToRange span)
                _ -> do
                  corefnExprMb <- liftLsp $ getCoreFnExprAt filePath pos
                  case corefnExprMb of
                    Just (CF.Var (ss, _comments, _meta) (P.Qualified qb ident)) -> do
                      liftLsp $ logDebugN $ "Found Corefn Var source span: " <> show ss
                      let name = P.runIdent ident
                      case qb of
                        P.ByModuleName coreMName -> do
                          declMb <- liftLsp $ getEfDeclarationInModule coreMName name
                          forLsp declMb \decl -> do
                            modFpMb <- liftLsp $ selectExternPathFromModuleName coreMName
                            forLsp modFpMb \modFp -> do
                              let sourceSpan = efDeclSourceSpan decl
                              locationRes modFp (spanToRange sourceSpan)
                        P.BySourcePos srcPos ->
                          locationRes filePath (Types.Range (sourcePosToPosition srcPos) (sourcePosToPosition srcPos))
                    _ -> nullRes,
      Server.requestHandler Message.SMethod_TextDocumentCompletion $ \req res -> do
        liftLsp $ logDebugN "SMethod_TextDocumentCompletion"
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

        liftLsp $ logDebugN $ "Completion params: " <> show completionCtx
        liftLsp $ logDebugN $ "filePathMb: " <> show filePathMb
        forLsp filePathMb \filePath -> do
          vfMb <- Server.getVirtualFile uri
          forLsp vfMb \vf -> do
            let word = getWordAt (VFS._file_text vf) pos
            liftLsp $ logDebugN $ "Word: " <> show word <> " len " <> show (T.length word)
            if T.length word < 2
              then nullRes
              else do
                mNameMb <- liftLsp $ selectExternModuleNameFromFilePath filePath
                liftLsp $ logDebugN $ "Module name: " <> show mNameMb
                forLsp mNameMb \mName -> do
                  decls <- liftLsp $ getAstDeclarationsStartingWith mName word
                  liftLsp $ logDebugN $ "Found decls: " <> show decls
                  declDocs <-
                    Map.fromList . catMaybes <$> forM decls \(declModule, decl) -> do
                      let name = printName <$> P.declName decl
                      docsMb <- liftLsp $ maybe (pure Nothing) (readDeclarationDocsAsMarkdown declModule) name
                      pure $ (decl,) <$> docsMb
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
                                  _documentation =
                                    Types.InR . Types.MarkupContent Types.MarkupKind_Markdown
                                      <$> (Map.lookup decl declDocs <|> fmap wrapPursMd (printDeclarationTypeMb decl)),
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
                                  _data_ = Just $ A.toJSON (mName, declModule, label) --  Maybe aeson-2.0.3.0:Data.Aeson.Types.Internal.Value
                                },
      Server.requestHandler Message.SMethod_CompletionItemResolve $ \req res -> do
        let completionItem = req ^. LSP.params
        -- filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
        -- uri :: Types.NormalizedUri
        -- uri =
        --   req
        --     ^. LSP.params
        --       . LSP.textDocument
        --       . LSP.uri
        --       . to Types.toNormalizedUri
        liftLsp $ logDebugN "SMethod_TextDocumentCompletionItemResolve"
        res $ Right (set LSP.documentation Nothing completionItem)
    ]
  where
    getFileDiagnotics :: (LSP.HasUri a2 Uri, LSP.HasTextDocument a1 a2, LSP.HasParams s a1) => s -> HandlerM config ([ErrorMessage], [Types.Diagnostic])
    getFileDiagnotics msg = do
      let uri :: Uri
          uri = getMsgUri msg
          fileName = Types.uriToFilePath uri
      case fileName of
        Just file -> do
          res <- fmap snd <$> liftLspWithErr (rebuildFile file)
          getResultDiagnostics res
        Nothing -> do
          sendInfoMsg $ "No file path for uri: " <> show uri
          pure ([], [])

    getMsgUri :: (LSP.HasParams s a1, LSP.HasTextDocument a1 a2, LSP.HasUri a2 a3) => s -> a3
    getMsgUri msg = msg ^. LSP.params . LSP.textDocument . LSP.uri

    getResultDiagnostics :: Either IdeError MultipleErrors -> HandlerM config ([ErrorMessage], [Types.Diagnostic])
    getResultDiagnostics res = case res of
      Left (RebuildError _ errs) -> do
        let errors = runMultipleErrors errs
            diags = errorMessageDiagnostic Types.DiagnosticSeverity_Error <$> errors
        pure (errors, diags)
      Left err -> do
        liftLsp $ logErrorN $ "Rebuild error: " <> textError err
        pure ([], [])
      Right errs | Errors.nonEmpty errs -> do
        let errors = runMultipleErrors errs
            diags = errorMessageDiagnostic Types.DiagnosticSeverity_Warning <$> errors
        pure (errors, diags)
      _ -> pure ([], [])
      where
        errorMessageDiagnostic :: Types.DiagnosticSeverity -> ErrorMessage -> Types.Diagnostic
        errorMessageDiagnostic severity msg@((ErrorMessage hints _)) =
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

sendError :: IdeError -> HandlerM config ()
sendError err =
  Server.sendNotification
    Message.SMethod_WindowShowMessage
    ( Types.ShowMessageParams Types.MessageType_Error $
        "Something went wrong:\n" <> textError err
    )

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
        configSection = "oa-purescript-simple",
        doInitialize = \env _req -> do
          logT "Init OA purs lsp server"
          pure $ Right env,
        staticHandlers = \_caps -> do handlers diagErrs,
        interpretHandler = \env ->
          Server.Iso
            ( runErrLogger LogAll
                . flip runReaderT lspEnv
                . Server.runLspT env
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

spy :: (Show a) => Text -> a -> a
spy msg a = unsafePerformIO $ do
  logT $ msg <> ": " <> show a
  pure a

unsafeLog :: (Show a) => a -> ()
unsafeLog a = unsafePerformIO $ log_ a

log_ :: (MonadIO m, Show a) => a -> m ()
log_ = logToFile . show

logT :: (MonadIO m) => Text -> m ()
logT = logToFile

logToFile :: (MonadIO m) => Text -> m ()
logToFile txt =
  liftIO $
    catchError
      ( do
          createDirectoryIfMissing True "logs"
          time <- show <$> getCurrentTime
          writeFile ("logs/" <> time <> "-----" <> T.unpack txt) $ txt <> "\n"
      )
      (const $ pure ())

wrapPursMd :: Text -> Text
wrapPursMd txt = "```purescript\n" <> txt <> "\n```"

-- getCompletionsWithPrim ::
--   (Ide m) =>
--   [Filter] ->
--   Matcher IdeDeclarationAnn ->
--   Maybe P.ModuleName ->
--   Purs.Completion.CompletionOptions ->
--   m [Completion]
-- getCompletionsWithPrim filters matcher currentModule complOptions = do
--   modules <- getAllModules currentModule
--   let insertPrim = Map.union idePrimDeclarations
--   pure (getCompletions filters matcher complOptions (insertPrim modules))

-- getExactCompletionsWithPrim ::
--   (Ide m) =>
--   Text ->
--   [Filter] ->
--   Maybe P.ModuleName ->
--   m [Completion]
-- getExactCompletionsWithPrim search filters currentModule = do
--   modules <- getAllModules currentModule
--   let insertPrim = Map.union idePrimDeclarations
--   pure (getExactCompletions search filters (insertPrim modules))
-- z = getAllModules
