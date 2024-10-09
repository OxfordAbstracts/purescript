{-# LANGUAGE TypeOperators #-}

module Language.PureScript.Lsp.State
  ( cacheRebuild,
    cacheRebuild',
    cachedRebuild,
    removedCachedRebuild,
    buildExportEnvCache,
    addExternToExportEnv,
    getExportEnv,
    cancelRequest,
    addRunningRequest,
    removeRunningRequest
  )
where

import Control.Concurrent.STM (TVar, modifyTVar, readTVar, readTVarIO, writeTVar)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as T
import Language.LSP.Protocol.Types (type (|?) (..))
import Language.LSP.Server (MonadLsp)
import Language.PureScript (MultipleErrors, prettyPrintMultipleErrors)
import Language.PureScript.Environment qualified as P
import Language.PureScript.Errors qualified as P
import Language.PureScript.Externs (ExternsFile (..))
import Language.PureScript.Externs qualified as P
import Language.PureScript.Lsp.Log (errorLsp)
import Language.PureScript.Lsp.ServerConfig (ServerConfig, getMaxFilesInCache)
import Language.PureScript.Lsp.Types
import Language.PureScript.Sugar.Names (externsEnv)
import Language.PureScript.Sugar.Names.Env qualified as P
import Protolude hiding (moduleName, unzip)

-- | Sets rebuild cache to the given ExternsFile
cacheRebuild :: (MonadReader LspEnvironment m, MonadLsp ServerConfig m) => ExternsFile -> [ExternsFile] -> P.Environment -> P.Environment -> m ()
cacheRebuild ef deps prevEnv finalEnv = do
  st <- lspStateVar <$> ask
  maxFiles <- getMaxFilesInCache
  liftIO $ cacheRebuild' st maxFiles ef deps prevEnv finalEnv

cacheRebuild' :: TVar LspState -> Int -> ExternsFile -> [P.ExternsFile] -> P.Environment -> P.Environment -> IO ()
cacheRebuild' st maxFiles ef deps prevEnv finalEnv = atomically . modifyTVar st $ \x ->
  x
    { openFiles = List.take maxFiles $ (fp, OpenFile (efModuleName ef) ef deps prevEnv finalEnv) : filter ((/= fp) . fst) (openFiles x)
    }
  where
    fp = P.spanName $ efSourceSpan ef

cachedRebuild :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> m (Maybe OpenFile)
cachedRebuild fp = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ do
    st' <- readTVar st
    pure $ List.lookup fp $ openFiles st'

removedCachedRebuild :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> m ()
removedCachedRebuild fp = do
  st <- lspStateVar <$> ask
  liftIO . atomically . modifyTVar st $ \x ->
    x
      { openFiles = filter ((/= fp) . fst) (openFiles x)
      }

buildExportEnvCache :: (MonadIO m, MonadReader LspEnvironment m, MonadThrow m) => P.Module -> [ExternsFile] -> m P.Env
buildExportEnvCache module' externs = do
  st <- lspStateVar <$> ask
  result <- liftIO . atomically $ do
    st' <- readTVar st
    if Map.member (P.getModuleName module') (exportEnv st')
      then pure $ Right $ exportEnv st'
      else do
        let notInEnv :: ExternsFile -> Bool
            notInEnv = flip Map.notMember (exportEnv st') . efModuleName
        result <- addExterns (exportEnv st') (filter notInEnv externs)
        case result of
          Left err -> pure $ Left err
          Right newEnv -> do
            writeTVar st $ st' {exportEnv = newEnv}
            pure $ Right newEnv

  case result of
    Left err -> throwM $ BuildEnvCacheException $ printBuildErrors err
    Right env -> pure env

data BuildEnvCacheException = BuildEnvCacheException Text
  deriving (Show)

instance Exception BuildEnvCacheException

addExterns :: (Foldable t, Monad f) => P.Env -> t ExternsFile -> f (Either MultipleErrors P.Env)
addExterns env externs = fmap fst . runWriterT $ runExceptT $ foldM externsEnv env externs

logBuildErrors :: (MonadIO m, MonadReader LspEnvironment m) => MultipleErrors -> m ()
logBuildErrors = errorLsp . printBuildErrors

printBuildErrors :: MultipleErrors -> Text
printBuildErrors = T.pack . prettyPrintMultipleErrors P.noColorPPEOptions

addExternToExportEnv :: (MonadIO m, MonadReader LspEnvironment m) => ExternsFile -> m ()
addExternToExportEnv ef = do
  stVar <- lspStateVar <$> ask
  error <- liftIO $ atomically $ do
    st <- readTVar stVar
    result <- addExterns (exportEnv st) [ef]
    case result of
      Left err -> pure $ Just err
      Right newEnv -> do
        writeTVar stVar $ st {exportEnv = newEnv}
        pure Nothing

  for_ error logBuildErrors

getExportEnv :: (MonadReader LspEnvironment m, MonadIO m) => m P.Env
getExportEnv = exportEnv <$> (liftIO . readTVarIO =<< lspStateVar <$> ask)

addRunningRequest :: (MonadIO m) => LspEnvironment -> Either Int32 Text -> Async () -> m ()
addRunningRequest env requestId req = liftIO . atomically $ modifyTVar (lspStateVar env) $ \x ->
  x
    { runningRequests = Map.insert requestId req (runningRequests x)
    }

removeRunningRequest :: (MonadIO m) => LspEnvironment -> Either Int32 Text -> m ()
removeRunningRequest env requestId = liftIO . atomically $ modifyTVar (lspStateVar env) $ \x ->
  x
    { runningRequests = Map.delete requestId (runningRequests x)
    }

cancelRequest :: (MonadReader LspEnvironment m, MonadIO m) => (Int32 |? Text) -> m ()
cancelRequest requestId = do
  st <- lspStateVar <$> ask
  reqMb <- liftIO . atomically $ do
    Map.lookup eitherId . runningRequests <$> readTVar st

  for_ reqMb $ \req -> liftIO $ cancel req
  where
    eitherId = case requestId of
      InL i -> Left i
      InR t -> Right t
