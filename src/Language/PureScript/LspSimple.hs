{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Language.PureScript.LspSimple (main) where

import Control.Concurrent.STM.TChan
import Control.Monad.IO.Unlift
import Data.IORef (newIORef)
import Data.Map qualified as Map
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server as LSP.Server
import Language.LSP.Server qualified as Server
import Language.PureScript.Lsp.Handlers (DiagnosticErrors, HandlerM, handlers)
import Language.PureScript.Lsp.Types (LspEnvironment)
import Protolude hiding (to)

main :: LspEnvironment -> IO Int
main lspEnv = do
  rin <- atomically newTChan :: IO (TChan ReactorInput)
  diagErrs <- newIORef Map.empty
  Server.runServer $
    Server.ServerDefinition
      { parseConfig = const $ const $ Right (),
        onConfigChange = const $ pure (),
        defaultConfig = (),
        configSection = "oa-purescript-lsp",
        doInitialize = \env _ -> forkIO (reactor rin) >> pure (Right env),
        staticHandlers = \_caps -> lspHandlers lspEnv diagErrs rin,
        interpretHandler = \serverEnv ->
          Server.Iso
            ( Server.runLspT serverEnv . flip runReaderT lspEnv
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

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

newtype ReactorInput
  = ReactorAction (IO ())

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: TChan ReactorInput -> IO ()
reactor inp = do
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: LspEnvironment -> DiagnosticErrors -> TChan ReactorInput -> Handlers (HandlerM ())
lspHandlers lspEnv diagErrors rin = mapHandlers goReq goNot (handlers diagErrors)
  where
    goReq :: forall (a :: LSP.Method LSP.ClientToServer LSP.Request). LSP.Server.Handler (HandlerM ()) a -> LSP.Server.Handler (HandlerM ()) a
    goReq f msg k = do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runHandler env $ f msg k)

    goNot :: forall (a :: LSP.Method LSP.ClientToServer LSP.Notification). LSP.Server.Handler (HandlerM ()) a -> LSP.Server.Handler (HandlerM ()) a
    goNot f msg = do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runHandler env $ f msg)

    runHandler env a = runLspT env $ runReaderT a lspEnv
