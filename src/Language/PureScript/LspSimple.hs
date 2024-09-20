{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.PureScript.LspSimple (main) where

import Control.Lens ((^.))
import Control.Monad.IO.Unlift
import Control.Monad.Reader (mapReaderT)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import GHC.IO (unsafePerformIO)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types (Diagnostic, Uri)
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server (getConfig)
import Language.LSP.Server qualified as Server
import Language.PureScript.Errors (ErrorMessage (ErrorMessage), MultipleErrors (runMultipleErrors), errorCode, errorDocUri, errorSpan, noColorPPEOptions, prettyPrintSingleError)
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Errors.JSON (toSuggestion)
import Language.PureScript.Errors.JSON qualified as JsonErrors
import Language.PureScript.Ide (findAvailableExterns, loadModulesAsync)
import Language.PureScript.Ide.Error (IdeError (RebuildError), textError)
import Language.PureScript.Ide.Rebuild (rebuildFileAsync)
import Language.PureScript.Ide.Types (Completion, IdeConfiguration (confLogLevel), IdeEnvironment (ideConfiguration), Success (RebuildSuccess, TextResult), IdeDeclarationAnn, Ide)
import Language.PureScript.Ide.Util (runLogger)
import Protolude
import System.Directory (createDirectoryIfMissing)
import Text.PrettyPrint.Boxes (render)
import "monad-logger" Control.Monad.Logger (LoggingT, mapLoggingT)
import Language.PureScript.Ide.Matcher (Matcher)
import Language.PureScript qualified as P
import Language.PureScript.Ide.Filter (Filter)
import Language.PureScript.Ide.Completion (getExactCompletions, getCompletions)
import Language.PureScript.Ide.Prim (idePrimDeclarations)
import Language.PureScript.Ide.State (getAllModules)
import Language.PureScript.Ide.Completion qualified as Purs.Completion

type HandlerM config = Server.LspT config (ReaderT IdeEnvironment (LoggingT IO))

type IdeM = ReaderT IdeEnvironment (LoggingT (ExceptT IdeError IO))

liftIde :: IdeM a -> HandlerM config (Either IdeError a)
liftIde = lift . mapReaderT (mapLoggingT runExceptT)

type DiagnosticErrors = IORef (Map Diagnostic ErrorMessage)

insertDiagnosticError :: (MonadIO m, Ord k) => IORef (Map k a) -> a -> k -> m ()
insertDiagnosticError diagErrs err diag = liftIO $ modifyIORef diagErrs (Map.insert diag err)

insertDiagnosticErrors :: (MonadIO m, Ord k) => IORef (Map k a) -> [a] -> [k] -> m ()
insertDiagnosticErrors diagErrs errs diags = liftIO $ modifyIORef diagErrs (Map.union $ Map.fromList $ zip diags errs)

getDiagnosticError :: (MonadIO m, Ord k) => IORef (Map k a) -> k -> m (Maybe a)
getDiagnosticError diagErrs diags = liftIO $ Map.lookup diags <$> readIORef diagErrs

getDiagnosticErrors :: (MonadIO m, Ord k) => IORef (Map k a) -> [k] -> m (Map k a)
getDiagnosticErrors diagErrs diags = liftIO $ flip Map.restrictKeys (Set.fromList diags) <$> readIORef diagErrs

-- z = combin
handlers :: DiagnosticErrors -> Server.Handlers (HandlerM ())
handlers diagErrs =
  mconcat
    [ Server.notificationHandler Message.SMethod_Initialized $ \_not -> do
        void $ liftIde $ findAvailableExterns >>= loadModulesAsync
        log_ ("OA purs lsp server initialized" :: T.Text)
        sendInfoMsg "OA purs lsp server initialized",
      Server.notificationHandler Message.SMethod_TextDocumentDidOpen $ \msg -> do
        sendInfoMsg "TextDocumentDidOpen",
      Server.notificationHandler Message.SMethod_TextDocumentDidChange $ \msg -> do
        sendInfoMsg "TextDocumentDidChange",
      Server.notificationHandler Message.SMethod_TextDocumentDidSave $ \msg -> do
        sendInfoMsg "SMethod_TextDocumentDidSave",
      Server.notificationHandler Message.SMethod_WorkspaceDidChangeConfiguration $ \msg -> do
        cfg <- getConfig
        sendInfoMsg $ "Config changed: " <> show cfg,
      Server.notificationHandler Message.SMethod_SetTrace $ \msg -> do
        sendInfoMsg "SMethod_SetTrace",
      Server.requestHandler Message.SMethod_TextDocumentDiagnostic $ \req res -> do
        sendInfoMsg "SMethod_TextDocumentDiagnostic"
        (errs, diagnostics) <- getFileDiagnotics req
        insertDiagnosticErrors diagErrs errs diagnostics
        res $
          Right $
            Types.DocumentDiagnosticReport $
              Types.InL $
                Types.RelatedFullDocumentDiagnosticReport Types.AString Nothing diagnostics Nothing,
      Server.requestHandler Message.SMethod_TextDocumentCodeAction $ \req res -> do
        sendInfoMsg "SMethod_TextDocumentCodeAction"
        let params = req ^. LSP.params
            -- doc = params ^. LSP.textDocument
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
                        & spy "suggestion"
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
        sendInfoMsg "SMethod_TextDocumentHover"
        let Types.HoverParams _doc pos _workDone = req ^. LSP.params
            Types.Position _l _c' = pos
            -- LSP.at
        res $
          Right $
            Types.InL $
              Types.Hover
                ( Types.InL $
                    Types.MarkupContent Types.MarkupKind_PlainText "Hello!"
                )
                Nothing,
      Server.requestHandler Message.SMethod_TextDocumentDocumentSymbol $ \req res -> do
        sendInfoMsg "SMethod_TextDocumentDocumentSymbol"
        -- getCompletionsWithPrim
        res $
          Right $
            Types.InL
              []
    ]
  where
    -- Types.DocumentSymbol
    --   "symbol"
    --   Nothing
    --   Types.SymbolKind_Array
    --   (Types.Range (Types.Position 0 0) (Types.Position 0 0))
    --   (Types.Range (Types.Position 0 0) (Types.Position 0 0))
    --   []

    getFileDiagnotics :: (LSP.HasUri a2 Uri, LSP.HasTextDocument a1 a2, LSP.HasParams s a1) => s -> HandlerM config ([ErrorMessage], [Types.Diagnostic])
    getFileDiagnotics msg = do
      let uri :: Uri
          uri = getMsgUri msg
          fileName = Types.uriToFilePath uri
      logT $ "Rebuilding file: " <> show (uri, fileName)
      case fileName of
        Just file -> do
          res <- liftIde $ rebuildFile file
          getResultDiagnostics res
        Nothing -> do
          sendInfoMsg $ "No file path for uri: " <> show uri
          pure ([], [])

    getMsgUri :: (LSP.HasParams s a1, LSP.HasTextDocument a1 a2, LSP.HasUri a2 a3) => s -> a3
    getMsgUri msg = msg ^. LSP.params . LSP.textDocument . LSP.uri

    getResultDiagnostics :: Either IdeError Success -> HandlerM config ([ErrorMessage], [Types.Diagnostic])
    getResultDiagnostics res = case res of
      Right success ->
        case success of
          RebuildSuccess errs -> do
            let errors = runMultipleErrors errs
                diags = errorMessageDiagnostic Types.DiagnosticSeverity_Warning <$> errors
            pure (errors, diags)
          TextResult _ -> pure ([], [])
          _ -> pure ([], [])
      Left (RebuildError _ errs) -> do
        let errors = runMultipleErrors errs
            diags = errorMessageDiagnostic Types.DiagnosticSeverity_Error <$> errors
        pure (errors, diags)
      Left err -> do
        sendError err
        pure ([], [])
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

sendError :: IdeError -> HandlerM config ()
sendError err =
  Server.sendNotification
    Message.SMethod_WindowShowMessage
    ( Types.ShowMessageParams Types.MessageType_Error $
        "Something went wrong:\n" <> textError err
    )

rebuildFile :: FilePath -> IdeM Success
rebuildFile file = do
  rebuildFileAsync file Nothing mempty

sendInfoMsg :: (Server.MonadLsp config f) => Text -> f ()
sendInfoMsg msg = Server.sendNotification Message.SMethod_WindowShowMessage (Types.ShowMessageParams Types.MessageType_Info msg)

main :: IdeEnvironment -> IO Int
main ideEnv = do
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
            ( runLogger (confLogLevel (ideConfiguration ideEnv))
                . flip runReaderT ideEnv
                . Server.runLspT env
            )
            liftIO,
        options = Server.defaultOptions
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
logToFile txt = liftIO $ do
  createDirectoryIfMissing True "logs"
  time <- show <$> getCurrentTime
  writeFile ("logs/" <> time <> "-----" <> T.unpack txt) $ txt <> "\n"

getCompletionsWithPrim ::
  (Ide m) =>
  [Filter] ->
  Matcher IdeDeclarationAnn ->
  Maybe P.ModuleName ->
  Purs.Completion.CompletionOptions ->
  m [Completion]
getCompletionsWithPrim filters matcher currentModule complOptions = do
  modules <- getAllModules currentModule
  let insertPrim = Map.union idePrimDeclarations
  pure (getCompletions filters matcher complOptions (insertPrim modules))

getExactCompletionsWithPrim ::
  (Ide m) =>
  Text ->
  [Filter] ->
  Maybe P.ModuleName ->
  m [Completion]
getExactCompletionsWithPrim search filters currentModule = do
  modules <- getAllModules currentModule
  let insertPrim = Map.union idePrimDeclarations
  pure (getExactCompletions search filters (insertPrim modules))