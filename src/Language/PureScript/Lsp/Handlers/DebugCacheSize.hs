{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}

module Language.PureScript.Lsp.Handlers.DebugCacheSize (debugCacheSizeHandler) where

import Data.Aeson qualified as A
import Data.Text qualified as T
import GHC.DataSize
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Server qualified as Server
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.State (getState)
import Language.PureScript.Lsp.Types (LspState (environments, openFiles), OpenFile (..))
import Numeric (showFFloat)
import Protolude hiding (to)

debugCacheSizeHandler :: Server.Handlers HandlerM
debugCacheSizeHandler =
  mconcat
    [ Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"debug-cache-size") $ \_req res -> do
        debugLsp "Debugging cache sizes"
        st <- getState
        for_ (openFiles st) \(fp, file@(OpenFile {..})) -> do
          debugSize (T.pack fp <> " - rebuild result") ofRebuildResult
          debugSize (T.pack fp <> " - artifacts") ofArtifacts
          debugSize (T.pack fp <> " - Full file") file

        for_ (environments st) \((fp, _), (exportEnv, env)) -> do
          debugSize (T.pack fp <> " - Export env") exportEnv
          debugSize (T.pack fp <> " - Environment") env

        debugLsp "Finished debugging cache sizes"

        res $ Right A.Null
    , Server.requestHandler (Message.SMethod_CustomMethod $ Proxy @"debug-cache-size-evaluated") $ \_req res -> do
        debugLsp "Debugging cache sizes"
        st <- getState
        for_ (openFiles st) \(fp, file@(OpenFile {..})) -> do
          debugSize (T.pack fp <> " - artifacts") ofArtifacts
          debugNfSize (T.pack fp <> " - artifacts") ofArtifacts
          debugSize (T.pack fp <> " - Full file") file

        for_ (environments st) \((fp, _), (_, env)) -> do
          debugSize (T.pack fp <> " - Environment") env
          debugNfSize (T.pack fp <> " - Environment") env

        debugLsp "Finished debugging cache sizes"

        res $ Right A.Null
    ]

debugSize :: Text -> a -> HandlerM ()
debugSize label a = do
  closure <- liftIO $ closureSize a
  debugLsp $
    label <> " - closure:\n" <> toMb closure

debugNfSize :: (NFData a) => Text -> a -> HandlerM ()
debugNfSize label a = do
  let !forced = force a
  !evaluated <- liftIO $ closureSize forced
  debugLsp $
    label <> " - evaluated:\n" <> toMb evaluated

toMb :: Word -> Text
toMb w =
  T.pack $
    formatFloatN
      ( fromIntegral w / 1e6
      )
      <> "MB"

formatFloatN :: Float -> [Char]
formatFloatN floatNum = showFFloat (Just 4) floatNum ""
