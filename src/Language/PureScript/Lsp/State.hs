{-# LANGUAGE PackageImports #-}
module Language.PureScript.Lsp.State where


import Control.Concurrent.STM (modifyTVar, readTVar)
import Language.PureScript.Externs (ExternsFile (..))
import Language.PureScript.Lsp.Types
import Protolude hiding (moduleName, unzip)
import Language.PureScript qualified as P

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