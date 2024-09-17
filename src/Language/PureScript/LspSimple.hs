{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.PureScript.LspSimple (main) where

import Control.Lens ((^.))
import Control.Monad.IO.Unlift
import Control.Monad.Reader (mapReaderT)
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types (Uri, toNormalizedUri)
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server (getConfig, publishDiagnostics)
import Language.LSP.Server qualified as Server
import Language.PureScript.Errors (ErrorMessage (ErrorMessage), ErrorMessageHint, MultipleErrors (runMultipleErrors), defaultPPEOptions, errorCode, errorDocUri, errorSpan, prettyPrintSingleError)
import Language.PureScript.Errors qualified as Errors
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
      --  Message.serverMethodJSON Message.SMethod_TextDocumentPublishDiagnostics _,
      --  Message.regHelper Message.SMethod_TextDocumentPublishDiagnostics _,
      --  $ \msg -> do
      --   sendInfoMsg "SMethod_TextDocumentPublishDiagnostics",
      Server.requestHandler Message.SMethod_TextDocumentDiagnostic $ \msg res -> do
        sendInfoMsg "SMethod_TextDocumentDiagnostic"
        diags <- getFileDiagnotics msg
        res $
          Right $
            Types.DocumentDiagnosticReport $
              Types.InL $
                Types.RelatedFullDocumentDiagnosticReport Types.AString Nothing diags Nothing
                --  $ \msg -> do
                --   sendInfoMsg "SMethod_TextDocumentDiagnostic"
    ]

rebuildFileFromMsg :: (LSP.HasParams s a1, LSP.HasTextDocument a1 a2, LSP.HasUri a2 Uri) => s -> HandlerM config ()
rebuildFileFromMsg msg = do
  let doc :: Uri
      doc = getDocument msg
      fileName = Types.uriToFilePath doc
  case fileName of
    Just file -> do
      res <- runIde $ rebuildFile file
      sendDiagnostics doc res
    Nothing ->
      sendInfoMsg $ "No file path for uri: " <> show doc

getFileDiagnotics :: (LSP.HasUri a2 Uri, LSP.HasTextDocument a1 a2, LSP.HasParams s a1) => s -> HandlerM config [Types.Diagnostic]
getFileDiagnotics msg = do
  let doc :: Uri
      doc = getDocument msg
      fileName = Types.uriToFilePath doc
  case fileName of
    Just file -> do
      res <- runIde $ rebuildFile file
      getResultDiagnostics doc res
    Nothing -> do
      sendInfoMsg $ "No file path for uri: " <> show doc
      pure []

rebuildFile :: FilePath -> IdeM Success
rebuildFile file = rebuildFileAsync file Nothing mempty

getDocument :: (LSP.HasParams s a1, LSP.HasTextDocument a1 a2, LSP.HasUri a2 a3) => s -> a3
getDocument msg = msg ^. LSP.params . LSP.textDocument . LSP.uri

sendDiagnostics :: Uri -> Either IdeError Success -> HandlerM config ()
sendDiagnostics uri res = do
  diags <- getResultDiagnostics uri res
  publishDiagnostics 100 (toNormalizedUri uri) Nothing (partitionBySource diags)

getResultDiagnostics :: Uri -> Either IdeError Success -> HandlerM config [Types.Diagnostic]
getResultDiagnostics uri res = case res of
  Right success ->
    case success of
      RebuildSuccess errs -> pure $ errorMessageDiagnostic <$> runMultipleErrors errs
      TextResult _ -> pure []
      _ -> pure []
  Left (RebuildError _ errs) -> pure $ errorMessageDiagnostic <$> runMultipleErrors errs
  Left err -> do
    sendError err
    pure []
  where
    errorMessageDiagnostic :: ErrorMessage -> Types.Diagnostic
    errorMessageDiagnostic msg@((ErrorMessage hints _)) =
      Types.Diagnostic
        (Types.Range start end)
        (Just Types.DiagnosticSeverity_Error)
        (Just $ Types.InR $ errorCode msg)
        (Just $ Types.CodeDescription $ Types.Uri $ errorDocUri msg)
        (T.pack <$> spanName)
        (T.pack $ render $ prettyPrintSingleError defaultPPEOptions msg)
        Nothing
        (Just $ hintToRelated <$> hints)
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

        hintToRelated :: Errors.ErrorMessageHint -> Types.DiagnosticRelatedInformation
        hintToRelated hint =
          Types.DiagnosticRelatedInformation
            (Types.Location uri (Types.Range hintStart hintEnd))
            (show hint)
          where
            (_, hintStart, hintEnd) = fromMaybe (Nothing, start, end) $ getPositionsMb $ getHintSpans hint

getHintSpans :: ErrorMessageHint -> Maybe (NEL.NonEmpty Errors.SourceSpan)
getHintSpans hint = case hint of
  Errors.PositionedError span -> Just span
  Errors.RelatedPositions span -> Just span
  _ -> Nothing

sendError :: IdeError -> HandlerM config ()
sendError err =
  Server.sendNotification
    Message.SMethod_WindowShowMessage
    ( Types.ShowMessageParams Types.MessageType_Error $
        "Something went wrong:\n" <> textError err
    )

sendInfoMsg :: (Server.MonadLsp config f) => Text -> f ()
sendInfoMsg msg = Server.sendNotification Message.SMethod_WindowShowMessage (Types.ShowMessageParams Types.MessageType_Info msg)

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