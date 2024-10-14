{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Language.PureScript.Lsp (main, serverDefinition) where

import Control.Concurrent.Async.Lifted (AsyncCancelled (AsyncCancelled))
import Control.Monad.IO.Unlift
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Text qualified as T
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript.Lsp.Handlers (handlers)
import Language.PureScript.Lsp.Log (debugLsp, errorLsp, warnLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.ServerConfig (ServerConfig, defaultFromEnv)
import Language.PureScript.Lsp.State (addRunningRequest, removeRunningRequest)
import Language.PureScript.Lsp.Types (LspEnvironment)
import Protolude hiding (to)
import Language.LSP.Server (mapHandlers, MonadLsp (getLspEnv))

main :: LspEnvironment -> IO Int
main lspEnv = do
  Server.runServer $ serverDefinition lspEnv

serverDefinition :: LspEnvironment -> Server.ServerDefinition ServerConfig
serverDefinition lspEnv =
  Server.ServerDefinition
    { parseConfig = \_current json -> first T.pack $ A.parseEither A.parseJSON json,
      onConfigChange = const $ pure (),
      defaultConfig = defaultFromEnv lspEnv,
      configSection = "oa-purescript-lsp",
      doInitialize = \env _ -> pure (Right env),
      staticHandlers = const (lspHandlers lspEnv),
      interpretHandler = \serverEnv ->
        Server.Iso
          ( Server.runLspT serverEnv . flip runReaderT lspEnv
          )
          liftIO,
      options = lspOptions
    }

lspOptions :: Server.Options
lspOptions =
  Server.defaultOptions
    { Server.optTextDocumentSync = Just syncOptions,
      Server.optExecuteCommandCommands = Just ["lsp-purescript-command"],
      Server.optCompletionTriggerCharacters = Just $ "._" <> ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] 
    }

syncOptions :: Types.TextDocumentSyncOptions
syncOptions =
  Types.TextDocumentSyncOptions
    { Types._openClose = Just True,
      Types._change = Just Types.TextDocumentSyncKind_Incremental,
      Types._willSave = Just False,
      Types._willSaveWaitUntil = Just False,
      Types._save = Just $ Types.InL True
    }

lspHandlers :: LspEnvironment -> Server.Handlers HandlerM
lspHandlers lspEnv = mapHandlers goReq goNotification handlers
  where
    goReq :: forall (a :: LSP.Method LSP.ClientToServer LSP.Request). Server.Handler HandlerM a -> Server.Handler HandlerM a
    goReq f msg@(LSP.TRequestMessage _ id method _) k = do
      let reqId = case id of
            LSP.IdInt i -> Left i
            LSP.IdString t -> Right t
      env <- getLspEnv
      debugLsp $ "Request: " <> show method
      liftIO $ do
        withAsync (runHandler env $ f msg k) \asyncAct -> do
          addRunningRequest lspEnv reqId asyncAct
          result <- waitCatch asyncAct
          runHandler env case result of
            Left e -> do
              case fromException e of
                Just AsyncCancelled -> do
                  warnLsp $ "Request cancelled. Method: " <> show method <> ". id: " <> show reqId
                  k $ Left $ LSP.TResponseError (Types.InL Types.LSPErrorCodes_RequestCancelled) "Cancelled" Nothing
                _ -> do
                  errorLsp $ "Request failed. Method: " <> show method <> ". id: " <> show reqId <> ". Error: " <> show e
                  k $ Left $ LSP.TResponseError (Types.InR Types.ErrorCodes_InternalError) "Internal error" Nothing
            _ -> pure ()
          removeRunningRequest lspEnv reqId

    goNotification :: forall (a :: LSP.Method LSP.ClientToServer LSP.Notification). Server.Handler HandlerM a -> Server.Handler HandlerM a
    goNotification f msg@(LSP.TNotificationMessage _ method _) = do
      env <- getLspEnv
      liftIO $ withAsync (runHandler env $ f msg) \asyncAct -> do
        result <- waitCatch asyncAct
        case result of
          Left e -> do
            runHandler env case fromException e of
              Just AsyncCancelled -> do
                warnLsp $ "Notification cancelled. Method: " <> show method
              _ -> do
                errorLsp $ "Notification failed. Method: " <> show method <> ". Error: " <> show e
          _ -> pure ()

    runHandler env a = Server.runLspT env $ runReaderT a lspEnv
