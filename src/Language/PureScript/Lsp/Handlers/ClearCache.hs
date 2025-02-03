{-# LANGUAGE TypeApplications #-}
module Language.PureScript.Lsp.Handlers.ClearCache where

import Protolude

import Data.Aeson qualified as A
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Server qualified as Server
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.State (clearCache, clearRebuildCache, clearEnvCache)

clearCacheHandlers :: Server.Handlers HandlerM

clearCacheHandlers =
  mconcat
    [ Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"clear-cache") $ \_req res -> do
        clearCache
        res $ Right A.Null,
      Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"clear-cache:environments") $ \_req res -> do
        clearEnvCache
        res $ Right A.Null,
      Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"clear-cache:rebuilds") $ \_req res -> do
        clearRebuildCache
        res $ Right A.Null
    ]