{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.PureScript.LspSimple (main) where

import Codec.Serialise (serialise, deserialise)
import Control.Lens ((^.))
import Control.Monad.IO.Unlift
import Control.Monad.Reader (mapReaderT)
import Data.Aeson qualified as A
import Data.ByteArray qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types (Uri, toNormalizedUri)
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server (getConfig, publishDiagnostics)
import Language.LSP.Server qualified as Server
import Language.PureScript.Errors (ErrorMessage (ErrorMessage), MultipleErrors (runMultipleErrors), errorCode, errorDocUri, errorSpan, noColorPPEOptions, prettyPrintSingleError)
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Ide (findAvailableExterns, loadModulesAsync)
import Language.PureScript.Ide.Error (IdeError (RebuildError), textError)
import Language.PureScript.Ide.Rebuild (rebuildFileAsync)
import Language.PureScript.Ide.Types (IdeConfiguration (confLogLevel), IdeEnvironment (ideConfiguration), Success (RebuildSuccess, TextResult))
import Language.PureScript.Ide.Util (runLogger)
import Protolude
import Text.PrettyPrint.Boxes (render)
import "monad-logger" Control.Monad.Logger (LoggingT, mapLoggingT)

type HandlerM config = Server.LspT config (ReaderT IdeEnvironment (LoggingT IO))

type IdeM = ReaderT IdeEnvironment (LoggingT (ExceptT IdeError IO))

runIde :: IdeM a -> HandlerM config (Either IdeError a)
runIde = lift . mapReaderT (mapLoggingT runExceptT)

handlers :: Server.Handlers (HandlerM ())
handlers =
  mconcat
    [ Server.notificationHandler Message.SMethod_Initialized $ \_not -> do
        void $ runIde $ findAvailableExterns >>= loadModulesAsync
        log_ ("OA purs lsp server initialized" :: T.Text)
        sendInfoMsg "OA purs lsp server initialized",
      Server.notificationHandler Message.SMethod_TextDocumentDidOpen $ \msg -> do
        sendInfoMsg "TextDocumentDidOpen"
        rebuildFileFromMsg msg,
      Server.notificationHandler Message.SMethod_TextDocumentDidChange $ \msg -> do
        sendInfoMsg "TextDocumentDidChange"
        rebuildFileFromMsg msg,
      Server.notificationHandler Message.SMethod_TextDocumentDidSave $ \msg -> do
        sendInfoMsg "SMethod_TextDocumentDidSave"
        rebuildFileFromMsg msg,
      Server.notificationHandler Message.SMethod_WorkspaceDidChangeConfiguration $ \msg -> do
        cfg <- getConfig
        sendInfoMsg $ "Config changed: " <> show cfg,
      Server.notificationHandler Message.SMethod_SetTrace $ \msg -> do
        sendInfoMsg "SMethod_SetTrace",
      Server.requestHandler Message.SMethod_TextDocumentDiagnostic $ \msg res -> do
        sendInfoMsg "SMethod_TextDocumentDiagnostic"
        diagnotics <- getFileDiagnotics msg
        res $
          Right $
            Types.DocumentDiagnosticReport $
              Types.InL $
                Types.RelatedFullDocumentDiagnosticReport Types.AString Nothing diagnotics Nothing,
      Server.requestHandler Message.SMethod_TextDocumentCodeAction $ \req res -> do
        sendInfoMsg "SMethod_TextDocumentCodeAction"
        let params = req ^. LSP.params
            -- doc = params ^. LSP.textDocument
            diags = params ^. LSP.context . LSP.diagnostics
        -- pure _
        -- diagnotics <- getFileDiagnotics msg
        res $
          Right $
            Types.InL $
              [ Types.InR $
                  Types.CodeAction
                    "Fix all"
                    (Just Types.CodeActionKind_QuickFix)
                    (Just diags)
                    (Just True)
                    Nothing -- disabled
                    (Just $ Types.WorkspaceEdit Nothing Nothing Nothing)
                    Nothing
                    Nothing
              ]
    ]

--     { _title       :: Text -- ^ A short, human-readable, title for this code action.
-- , _kind        :: Maybe CodeActionKind -- ^ The kind of the code action. Used to filter code actions.
-- , _diagnostics :: Maybe (List Diagnostic) -- ^ The diagnostics that this code action resolves.
-- , _edit        :: Maybe WorkspaceEdit -- ^ The workspace edit this code action performs.
-- , _command     :: Maybe Command -- ^ A command this code action executes. If a code action
--                                 -- provides an edit and a command, first the edit is
--                                 -- executed and then the command.
-- , _isPreferred :: Maybe Bool -- ^ Marks this as a preferred action.
--                           -- Preferred actions are used by the `auto fix` command and can be targeted by keybindings.
--                           -- A quick fix should be marked preferred if it properly addresses the underlying error.
--                           -- A refactoring should be marked preferred if it is the most reasonable choice of actions to take.
-- , _disabled    :: Maybe Reason -- ^ Marks that the code action cannot currently be applied.
-- }

rebuildFileFromMsg :: (LSP.HasParams s a1, LSP.HasTextDocument a1 a2, LSP.HasUri a2 Uri) => s -> HandlerM config ()
rebuildFileFromMsg msg = do
  let uri :: Uri
      uri = getMsgUri msg
      fileName = Types.uriToFilePath uri
  case fileName of
    Just file -> do
      res <- runIde $ rebuildFile file
      sendDiagnostics uri res
    Nothing ->
      sendInfoMsg $ "No file path for uri: " <> show uri

getFileDiagnotics :: (LSP.HasUri a2 Uri, LSP.HasTextDocument a1 a2, LSP.HasParams s a1) => s -> HandlerM config [Types.Diagnostic]
getFileDiagnotics msg = do
  let uri :: Uri
      uri = getMsgUri msg
      fileName = Types.uriToFilePath uri
  case fileName of
    Just file -> do
      res <- runIde $ rebuildFile file
      getResultDiagnostics uri res
    Nothing -> do
      sendInfoMsg $ "No file path for uri: " <> show uri
      pure []

rebuildFile :: FilePath -> IdeM Success
rebuildFile file = do
  rebuildFileAsync file Nothing mempty

getMsgUri :: (LSP.HasParams s a1, LSP.HasTextDocument a1 a2, LSP.HasUri a2 a3) => s -> a3
getMsgUri msg = msg ^. LSP.params . LSP.textDocument . LSP.uri

sendDiagnostics :: Uri -> Either IdeError Success -> HandlerM config ()
sendDiagnostics uri res = do
  diags <- getResultDiagnostics uri res
  publishDiagnostics 100 (toNormalizedUri uri) Nothing (partitionBySource diags)

getResultDiagnostics :: Uri -> Either IdeError Success -> HandlerM config [Types.Diagnostic]
getResultDiagnostics uri res = case res of
  Right success ->
    case success of
      RebuildSuccess errs -> pure $ errorMessageDiagnostic Types.DiagnosticSeverity_Warning <$> runMultipleErrors errs
      TextResult _ -> pure []
      _ -> pure []
  Left (RebuildError _ errs) -> pure $ errorMessageDiagnostic Types.DiagnosticSeverity_Error <$> runMultipleErrors errs
  Left err -> do
    sendError err
    pure []
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
        -- (Just $ encodeErrorMessage msg)
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

sendInfoMsg :: (Server.MonadLsp config f) => Text -> f ()
sendInfoMsg msg = Server.sendNotification Message.SMethod_WindowShowMessage (Types.ShowMessageParams Types.MessageType_Info msg)

encodeErrorMessage :: ErrorMessage -> A.Value
encodeErrorMessage msg = A.toJSON $ TE.decodeUtf8 $ B.concat $ BL.toChunks $ serialise msg

decodeErrorMessage :: A.Value -> Either Text ErrorMessage
decodeErrorMessage json = do
  fromJson :: Text <- case A.fromJSON json of
    A.Success a -> Right a
    A.Error err -> Left $ T.pack err
  deserialise $ toUtf8Lazy fromJson

  -- Left ""

--  fromJSON json & TE.encodeUtf8  & _ & BL.fromChunks $ _

main :: IdeEnvironment -> IO Int
main ideEnv =
  Server.runServer $
    Server.ServerDefinition
      { parseConfig = const $ const $ Right (),
        onConfigChange = const $ pure (),
        defaultConfig = (),
        configSection = "oa-purescript-simple",
        doInitialize = \env _req -> do
          logT "Init OA purs lsp server"
          pure $ Right env,
        staticHandlers = \_caps -> do handlers,
        interpretHandler = \env ->
          Server.Iso
            ( runLogger (confLogLevel (ideConfiguration ideEnv))
                . flip runReaderT ideEnv
                . Server.runLspT env
            )
            liftIO,
        options = Server.defaultOptions
      }

log_ :: (MonadIO m, Show a) => a -> m ()
log_ = logToFile "log.txt" . show

logT :: (MonadIO m) => Text -> m ()
logT = logToFile "log.txt"

logToFile :: (MonadIO m) => FilePath -> Text -> m ()
logToFile path txt = liftIO $ appendFile path $ txt <> "\n"