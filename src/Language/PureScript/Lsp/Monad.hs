{-# LANGUAGE InstanceSigs #-}

module Language.PureScript.Lsp.Monad where

import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Control (MonadBaseControl (StM, liftBaseWith, restoreM), RunInBase)
import Language.LSP.Server (LanguageContextEnv, LspT (LspT), MonadLsp, runLspT)
import Language.PureScript.Lsp.ServerConfig (ServerConfig)
import Language.PureScript.Lsp.Types
import Protolude

newtype HandlerM a = HandlerM
  { unHandlerM :: ReaderT LspEnvironment (LspT ServerConfig IO) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow, MonadReader LspEnvironment, MonadLsp ServerConfig)

instance MonadBase IO HandlerM where
  liftBase = liftIO

instance MonadBaseControl IO HandlerM where
  type StM HandlerM a = a

  liftBaseWith :: (RunInBase HandlerM IO -> IO a) -> HandlerM a
  liftBaseWith f = HandlerM $
    ReaderT $ \lspEnv ->
      LspT $
        ReaderT $
          \serverConfig ->
            liftBaseWith $ \q -> f $ q . runHandlerM serverConfig lspEnv

  restoreM :: StM HandlerM a -> HandlerM a
  restoreM = pure

runHandlerM :: LanguageContextEnv ServerConfig -> LspEnvironment -> HandlerM a -> IO a
runHandlerM env lspEnv (HandlerM a) = runLspT env $ runReaderT a lspEnv
