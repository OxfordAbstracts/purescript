{-# LANGUAGE PackageImports #-}
module Language.PureScript.Lsp.State where


import Control.Concurrent.STM (modifyTVar)
import Language.PureScript.Externs (ExternsFile (..))
import Language.PureScript.Lsp.Types
import Protolude hiding (moduleName, unzip)

-- | Sets rebuild cache to the given ExternsFile
cacheRebuild :: (MonadIO m, MonadReader LspEnvironment m) => ExternsFile -> m ()
cacheRebuild ef = do
  st <- lspStateVar <$> ask
  liftIO . atomically . modifyTVar st $ \x ->
    x
      { currentFile = Just $ CurrentFile (efModuleName ef) ef
      }
