{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

module Language.PureScript.Lsp.State where

import Control.Concurrent.STM (modifyTVar, readTVar)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.LSP.Protocol.Types (type (|?) (..))
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs (ExternsFile (..))
import Language.PureScript.Lsp.Types
import Protolude hiding (moduleName, unzip)

-- | Sets rebuild cache to the given ExternsFile
cacheRebuild :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> ExternsFile -> P.Environment -> m ()
cacheRebuild fp ef env = do
  st <- lspStateVar <$> ask
  liftIO . atomically . modifyTVar st $ \x ->
    x
      { openFiles = Map.insert fp (OpenFile (efModuleName ef) ef env) (openFiles x)
      }

cachedRebuild :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> m (Maybe OpenFile)
cachedRebuild fp = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ do
    st' <- readTVar st
    pure $ Map.lookup fp $ openFiles st'

cancelRequest :: (MonadReader LspEnvironment m, MonadIO m) => (Int32 |? Text) -> m ()
cancelRequest requestId = do
  st <- lspStateVar <$> ask
  liftIO . atomically . modifyTVar st $ \x ->
    x
      { cancelledRequests = Set.insert eitherId (cancelledRequests x)
      }
  where
    eitherId = case requestId of
      InL i -> Left i
      InR t -> Right t

requestIsCancelled :: (MonadReader LspEnvironment m, MonadIO m) => Either Int32 Text -> m Bool
requestIsCancelled requestId = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ do
    st' <- readTVar st
    pure $ requestId `Set.member` cancelledRequests st'