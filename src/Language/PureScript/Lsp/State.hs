{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

module Language.PureScript.Lsp.State where

import Control.Concurrent.STM (modifyTVar, readTVar)
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs (ExternsFile (..))
import Language.PureScript.Lsp.Types
import Protolude hiding (moduleName, unzip)
import Data.Set qualified as Set
import Language.LSP.Protocol.Types (type (|?)(..))
import Language.PureScript.Lsp.Log (debugLsp)
-- import Language.LSP.Protocol.Types ((InL))

-- | Sets rebuild cache to the given ExternsFile
cacheRebuild :: (MonadIO m, MonadReader LspEnvironment m) => ExternsFile -> P.Module -> P.Environment -> m ()
cacheRebuild ef module' env = do
  st <- lspStateVar <$> ask
  liftIO . atomically . modifyTVar st $ \x ->
    x
      { currentFile = Just $ CurrentFile (efModuleName ef) module' ef env
      }

cachedRebuild :: (MonadIO m, MonadReader LspEnvironment m) => m (Maybe CurrentFile)
cachedRebuild = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ do
    st' <- readTVar st
    pure $ currentFile st'


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
  cancelled <- liftIO . atomically $ do
    st' <- readTVar st
    pure $ requestId `Set.member` cancelledRequests st'
  debugLsp $ "Request " <> show requestId <> " is cancelled " <> show cancelled
  pure cancelled