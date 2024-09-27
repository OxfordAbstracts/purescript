{-# LANGUAGE PackageImports #-}
module Language.PureScript.Lsp.State where


import Control.Concurrent.STM (modifyTVar, readTVar)
import Language.PureScript.Externs (ExternsFile (..))
import Language.PureScript.Lsp.Types
import Protolude hiding (moduleName, unzip)
import Language.PureScript.AST.Declarations qualified as P

-- | Sets rebuild cache to the given ExternsFile
cacheRebuild :: (MonadIO m, MonadReader LspEnvironment m) => ExternsFile -> P.Module -> m ()
cacheRebuild ef module' = do
  st <- lspStateVar <$> ask
  liftIO . atomically . modifyTVar st $ \x ->
    x
      { currentFile = Just $ CurrentFile (efModuleName ef) module' ef
      }


cachedRebuild :: (MonadIO m, MonadReader LspEnvironment m) => m (Maybe CurrentFile)
cachedRebuild = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ do
    st' <- readTVar st
    pure $ currentFile st'


getInitialized :: (MonadIO m, MonadReader LspEnvironment m) => m Bool 
getInitialized = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ do
    st' <- readTVar st
    pure $ lspInitalized st'


initFinished :: (MonadIO m, MonadReader LspEnvironment m) => m ()
initFinished = do
  st <- lspStateVar <$> ask
  liftIO . atomically . modifyTVar st $ \x ->
    x
      { lspInitalized = True
      }

whenInitialized :: (MonadIO m, MonadReader LspEnvironment m) => m () -> m ()
whenInitialized action = do
  initialized <- getInitialized
  when initialized action

waitForInit :: (MonadIO m, MonadReader LspEnvironment m) => m ()
waitForInit = do
  initialized <- getInitialized
  unless initialized $ do
    liftIO $ threadDelay 100000
    waitForInit