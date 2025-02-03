{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}

module Language.PureScript.Lsp (main, serverDefinition) where

import Control.Concurrent.Async.Lifted (AsyncCancelled (AsyncCancelled))
import Control.Concurrent.Async.Lifted qualified as Lifted
import Control.Monad.IO.Unlift
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Text qualified as T
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server (mapHandlers)
import Language.LSP.Server qualified as Server
import Language.PureScript.DB (mkDbPath)
import Language.PureScript.Lsp.Cache (updateAvailableSrcs)
import Language.PureScript.Lsp.Handlers (handlers)
import Language.PureScript.Lsp.Log (debugLsp, errorLsp, logPerfStandard, warnLsp)
import Language.PureScript.Lsp.Monad (HandlerM, runHandlerM)
import Language.PureScript.Lsp.ServerConfig (ServerConfig (globs, outputPath), defaultConfig)
import Language.PureScript.Lsp.State (addRunningRequest, getDbPath, getPreviousConfig, putNewEnv, putPreviousConfig, removeRunningRequest)
import Language.PureScript.Lsp.Types (LspEnvironment)
import Protolude hiding (to)

main :: FilePath -> LspEnvironment -> IO Int
main outputPath lspEnv = do
  Server.runServer $ serverDefinition outputPath lspEnv

serverDefinition :: FilePath -> LspEnvironment -> Server.ServerDefinition ServerConfig
serverDefinition initialOutputPath lspEnv =
  Server.ServerDefinition
    { parseConfig = \_current json -> first T.pack $ A.parseEither A.parseJSON json,
      onConfigChange = \newConfig -> do
        dbPath <- getDbPath
        newDbPath <- liftIO $ mkDbPath (outputPath newConfig)
        when (newDbPath /= dbPath) do
          debugLsp "DB path changed"
          liftIO $ putNewEnv lspEnv $ outputPath newConfig
        prevConfig <- getPreviousConfig
        when (globs newConfig /= globs prevConfig) do
          debugLsp "Globs changed"
          void updateAvailableSrcs
        putPreviousConfig newConfig,
      defaultConfig = defaultConfig initialOutputPath,
      configSection = "purescript-lsp",
      doInitialize = \env _ -> pure (Right env),
      staticHandlers = const (lspHandlers lspEnv),
      interpretHandler = \serverEnv ->
        Server.Iso
          ( runHandlerM serverEnv lspEnv
          )
          liftIO,
      options = lspOptions
    }

lspOptions :: Server.Options
lspOptions =
  Server.defaultOptions
    { Server.optTextDocumentSync = Just syncOptions,
      Server.optCompletionTriggerCharacters = Just $ "._" <> ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
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
    goReq :: forall (a :: LSP.Method 'LSP.ClientToServer 'LSP.Request). Server.Handler HandlerM a -> Server.Handler HandlerM a
    goReq f msg@(LSP.TRequestMessage _ id method _) k = do
      let reqId = case id of
            LSP.IdInt i -> Left i
            LSP.IdString t -> Right t
          methodText = T.pack $ LSP.someMethodToMethodString $ LSP.SomeMethod method
      debugLsp methodText
      logPerfStandard methodText $ do
        Lifted.withAsync (f msg k) \asyncAct -> do
          addRunningRequest lspEnv reqId asyncAct
          result <- Lifted.waitCatch asyncAct
          case result of
            Left e -> do
              case fromException e of
                Just AsyncCancelled -> do
                  warnLsp $ "Request cancelled. Method: " <> methodText <> ". id: " <> show reqId
                  k $ Left $ LSP.TResponseError (Types.InL Types.LSPErrorCodes_RequestCancelled) "Cancelled" Nothing
                _ -> do
                  errorLsp $ "Request failed. Method: " <> methodText <> ". id: " <> show reqId <> ". Error: " <> show e
                  k $ Left $ LSP.TResponseError (Types.InR Types.ErrorCodes_InternalError) "Internal error" Nothing
            _ -> pure ()
          removeRunningRequest lspEnv reqId

    goNotification :: forall (a :: LSP.Method 'LSP.ClientToServer 'LSP.Notification). Server.Handler HandlerM a -> Server.Handler HandlerM a
    goNotification f msg@(LSP.TNotificationMessage _ method _) = do
      let methodText = T.pack $ LSP.someMethodToMethodString $ LSP.SomeMethod method
      Lifted.withAsync (f msg) \asyncAct -> do
        result <- Lifted.waitCatch asyncAct
        case result of
          Left e -> do
            case fromException e of
              Just AsyncCancelled -> do
                warnLsp $ "Notification cancelled. Method: " <> methodText
              _ -> do
                errorLsp $ "Notification failed. Method: " <> methodText <> ". Error: " <> show e
          _ -> pure ()