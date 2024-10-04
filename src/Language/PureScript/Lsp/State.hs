{-# LANGUAGE PackageImports #-}

module Language.PureScript.Lsp.State where

import Control.Concurrent.STM (modifyTVar, readTVar)
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs (ExternsFile (..))
import Language.PureScript.Lsp.Types
import Protolude hiding (moduleName, unzip)

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
