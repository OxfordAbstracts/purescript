{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.PureScript.Lsp (main, serverDefinition) where

import Control.Concurrent.STM.TChan
import Control.Monad.IO.Unlift
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Text qualified as T
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server as LSP.Server
import Language.LSP.Server qualified as Server
import Language.PureScript.Lsp.Handlers (handlers)
import Language.PureScript.Lsp.Log (logPerfStandard)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.ServerConfig (ServerConfig, defaultFromEnv)
import Language.PureScript.Lsp.State (requestIsCancelled)
import Language.PureScript.Lsp.Types (LspEnvironment)
import Protolude hiding (to)

main :: LspEnvironment -> IO Int
main lspEnv = do
  rin <- atomically newTChan :: IO (TChan ReactorInput)
  Server.runServer $ serverDefinition lspEnv rin

serverDefinition :: LspEnvironment -> TChan ReactorInput -> ServerDefinition ServerConfig
serverDefinition lspEnv _rin =
  Server.ServerDefinition
    { parseConfig = \_current json -> first T.pack $ A.parseEither A.parseJSON json,
      onConfigChange = const $ pure (),
      defaultConfig = defaultFromEnv lspEnv,
      configSection = "oa-purescript-lsp",
      doInitialize = \env _ -> pure (Right env),
      staticHandlers = const handlers,
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
      Server.optExecuteCommandCommands = Just ["lsp-purescript-command"]
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

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.
data ReactorInput = ReactorAction
  { riId :: Maybe (Either Int32 Text),
    riMethod :: Text,
    riAction :: IO ()
  }

-- | We have 3 channels for the 3 different types of requests we can receive
-- | As diagnostics and custom commands are often slow, we want to keep them
-- | separate from the standard requests
data Reactors = Reactors
  { standard :: ReactorInput,
    diagnostics :: ReactorInput,
    customCommands :: ReactorInput
  }

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: TChan ReactorInput -> IO ()
reactor inp = do
  forever $ do
    ReactorAction reqId method act <- atomically $ readTChan inp
    withAsync act \a -> do
      res <- waitCatch a
      case res of
        Left e ->
          putErrLn
            ( "Request failed. Method: "
                <> show method
                <> ". id: "
                <> show reqId
                <> ". Error: "
                <> show e ::
                Text
            )
        Right _ -> pure ()

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: LspEnvironment -> TChan ReactorInput -> Handlers (HandlerM ServerConfig)
lspHandlers lspEnv rin = mapHandlers goReq goNotification handlers
  where
    goReq :: forall (a :: LSP.Method LSP.ClientToServer LSP.Request). LSP.Server.Handler (HandlerM ServerConfig) a -> LSP.Server.Handler (HandlerM ServerConfig) a
    goReq f msg@(LSP.TRequestMessage _ id method _) k = do
      let reqId = case id of
            LSP.IdInt i -> Left i
            LSP.IdString t -> Right t

      writeToChannel (Just reqId) (show method) $
        ifM
          (requestIsCancelled reqId)
          (k $ Left $ LSP.TResponseError (Types.InL Types.LSPErrorCodes_RequestCancelled) "Cancelled" Nothing)
          (logPerfStandard ("Request " <> show method) $ f msg k)

    goNotification :: forall (a :: LSP.Method LSP.ClientToServer LSP.Notification). LSP.Server.Handler (HandlerM ServerConfig) a -> LSP.Server.Handler (HandlerM ServerConfig) a
    goNotification f msg@(LSP.TNotificationMessage _ LSP.SMethod_CancelRequest _) = do
      f msg -- cancel requests skip the queue and are handled immediately on the main thread
    goNotification f msg@(LSP.TNotificationMessage _ method _) = do
      writeToChannel Nothing (show method) (f msg)

    writeToChannel = writeToChannelWith writeTChan

    writeToChannelWith fn reqId method a = do
      env <- getLspEnv
      liftIO $ atomically $ fn rin $ ReactorAction reqId method (runHandler env a)

    runHandler env a = runLspT env $ runReaderT a lspEnv
