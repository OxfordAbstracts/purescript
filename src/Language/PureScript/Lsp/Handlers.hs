{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Language.PureScript.Lsp.Handlers where

import Control.Lens ((^.))
import Data.Aeson qualified as A
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript.Lsp.Cache (updateAvailableSrcs)
import Language.PureScript.Lsp.Diagnostics (getMsgUri)
import Language.PureScript.Lsp.Handlers.Build (buildHandler)
import Language.PureScript.Lsp.Handlers.Completion (completionAndResolveHandlers)
import Language.PureScript.Lsp.Handlers.Definition (definitionHandler)
import Language.PureScript.Lsp.Handlers.DeleteOutput (deleteOutputHandler)
import Language.PureScript.Lsp.Handlers.Diagnostic (diagnosticAndCodeActionHandlers)
import Language.PureScript.Lsp.Handlers.Hover (hoverHandler)
import Language.PureScript.Lsp.Handlers.Index (indexHandler)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.ReadFile (lspReadFileText)
import Language.PureScript.Lsp.ServerConfig (setTraceValue)
import Language.PureScript.Lsp.State (cancelRequest, clearCache, clearExportCache, clearRebuildCache, getDbConn, removedCachedRebuild)
import Language.PureScript.Make.Index (dropTables, initDb)
import Protolude hiding (to)
import System.Process (readProcess)

handlers :: Server.Handlers HandlerM
handlers =
  mconcat
    [ simpleHandlers,
      buildHandler,
      completionAndResolveHandlers,
      definitionHandler,
      deleteOutputHandler,
      diagnosticAndCodeActionHandlers,
      hoverHandler,
      indexHandler
    ]
  where
    -- Simple handlers that don't need to be in their own module
    simpleHandlers =
      mconcat
        [ Server.notificationHandler Message.SMethod_Initialized $ \_not -> do
            void updateAvailableSrcs
            sendInfoMsg "Lsp initialized",
          Server.notificationHandler Message.SMethod_WorkspaceDidChangeWatchedFiles $ \_not -> do
            pure (),
          Server.notificationHandler Message.SMethod_TextDocumentDidOpen $ \_msg -> do
            pure (),
          Server.notificationHandler Message.SMethod_TextDocumentDidChange $ \_msg -> do
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

          Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"clear-cache") $ \_req res -> do
            clearCache
            res $ Right A.Null,
          Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"clear-cache:exports") $ \_req res -> do
            clearExportCache
            res $ Right A.Null,
          Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"clear-cache:rebuilds") $ \_req res -> do
            clearRebuildCache
            res $ Right A.Null,
          Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"create-index-tables") $ \_req res -> do
            conn <- getDbConn
            liftIO $ initDb conn
            res $ Right A.Null,
          Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"drop-index-tables") $ \_req res -> do
            conn <- getDbConn
            liftIO $ dropTables conn
            res $ Right A.Null
        ]

sendInfoMsg :: (Server.MonadLsp config f) => Text -> f ()
sendInfoMsg msg = Server.sendNotification Message.SMethod_WindowShowMessage (Types.ShowMessageParams Types.MessageType_Info msg)
