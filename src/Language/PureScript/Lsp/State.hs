{-# LANGUAGE TypeOperators #-}

module Language.PureScript.Lsp.State
  ( getDbConn,
    cacheRebuild,
    cacheRebuild',
    cachedRebuild,
    cacheDependencies,
    clearCache,
    clearRebuildCache,
    clearExportCache,
    setExportEnvCache,
    removedCachedRebuild,
    buildExportEnvCache,
    addExternToExportEnv,
    addExternsToExportEnv,
    getExportEnv,
    cancelRequest,
    addRunningRequest,
    removeRunningRequest,
    getDbPath,
    putNewEnv,
  )
where

import Control.Concurrent.STM (TVar, modifyTVar, readTVar, readTVarIO, writeTVar)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import Language.LSP.Protocol.Types (type (|?) (..))
import Language.LSP.Server (MonadLsp)
import Language.PureScript (MultipleErrors, prettyPrintMultipleErrors)
import Language.PureScript.DB (mkConnection)
import Language.PureScript.Environment qualified as P
import Language.PureScript.Errors qualified as P
import Language.PureScript.Externs (ExternsFile (..))
import Language.PureScript.Externs qualified as P
import Language.PureScript.Lsp.Log (errorLsp)
import Language.PureScript.Lsp.ServerConfig (ServerConfig, getMaxFilesInCache)
import Language.PureScript.Lsp.Types
import Language.PureScript.Names qualified as P
import Language.PureScript.Sugar.Names (externsEnv)
import Language.PureScript.Sugar.Names.Env qualified as P
import Protolude hiding (moduleName, unzip)

getDbConn :: (MonadReader LspEnvironment m, MonadIO m) => m Connection
getDbConn = liftIO . fmap snd . readTVarIO . lspDbConnectionVar =<< ask

-- | Sets rebuild cache to the given ExternsFile
cacheRebuild :: (MonadReader LspEnvironment m, MonadLsp ServerConfig m) => ExternsFile -> [ExternsFile] -> P.Environment -> P.Module -> m ()
cacheRebuild ef deps prevEnv module' = do
  st <- lspStateVar <$> ask
  maxFiles <- getMaxFilesInCache
  liftIO $ cacheRebuild' st maxFiles ef deps prevEnv module'

cacheRebuild' :: TVar LspState -> Int -> ExternsFile -> [P.ExternsFile] -> P.Environment -> P.Module -> IO ()
cacheRebuild' st maxFiles ef deps prevEnv module' = atomically . modifyTVar st $ \x ->
  x
    { openFiles = List.take maxFiles $ (fp, OpenFile (efModuleName ef) ef deps prevEnv module') : filter ((/= fp) . fst) (openFiles x)
    }
  where
    fp = P.spanName $ efSourceSpan ef

cachedRebuild :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> m (Maybe OpenFile)
cachedRebuild fp = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ do
    st' <- readTVar st
    pure $ List.lookup fp $ openFiles st'

cacheDependencies :: (MonadReader LspEnvironment m, MonadLsp ServerConfig m) => P.ModuleName -> [ExternsFile] -> m ()
cacheDependencies moduleName deps = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ modifyTVar st $ \x ->
    x
      { openFiles =
          openFiles x <&> \(fp, ofile) ->
            if ofModuleName ofile == moduleName
              then (fp, ofile {ofDependencies = deps})
              else (fp, ofile)
      }

removedCachedRebuild :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> m ()
removedCachedRebuild fp = do
  st <- lspStateVar <$> ask
  liftIO . atomically . modifyTVar st $ \x ->
    x
      { openFiles = filter ((/= fp) . fst) (openFiles x)
      }

clearRebuildCache :: (MonadReader LspEnvironment m, MonadIO m) => m ()
clearRebuildCache = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ modifyTVar st $ \x -> x {openFiles = []}

clearExportCache :: (MonadReader LspEnvironment m, MonadIO m) => m ()
clearExportCache = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ modifyTVar st $ \x -> x {exportEnv = P.primEnv}

clearCache :: (MonadReader LspEnvironment m, MonadIO m) => m ()
clearCache = clearRebuildCache >> clearExportCache

buildExportEnvCache :: (MonadIO m, MonadReader LspEnvironment m) => P.Module -> [ExternsFile] -> m (Either MultipleErrors P.Env)
buildExportEnvCache module' externs = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ do
    st' <- readTVar st
    if Map.member (P.getModuleName module') (exportEnv st')
      then pure $ Right $ exportEnv st'
      else do
        let notInEnv :: ExternsFile -> Bool
            notInEnv = flip Map.notMember (exportEnv st') . efModuleName
        result <- addExternsToExportEnv (exportEnv st') (filter notInEnv externs)
        case result of
          Left err -> pure $ Left err
          Right newEnv -> do
            writeTVar st $ st' {exportEnv = newEnv}
            pure $ Right newEnv

setExportEnvCache :: (MonadIO m, MonadReader LspEnvironment m) => P.Env -> m ()
setExportEnvCache env = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ modifyTVar st $ \x -> x {exportEnv = env}

data BuildEnvCacheException = BuildEnvCacheException Text
  deriving (Show)

instance Exception BuildEnvCacheException

addExternsToExportEnv :: (Foldable t, Monad f) => P.Env -> t ExternsFile -> f (Either MultipleErrors P.Env)
addExternsToExportEnv env externs = fmap fst . runWriterT $ runExceptT $ foldM externsEnv env externs

logBuildErrors :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m) => MultipleErrors -> m ()
logBuildErrors = errorLsp . printBuildErrors

printBuildErrors :: MultipleErrors -> Text
printBuildErrors = T.pack . prettyPrintMultipleErrors P.noColorPPEOptions

addExternToExportEnv :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m) => ExternsFile -> m ()
addExternToExportEnv ef = do
  stVar <- lspStateVar <$> ask
  error <- liftIO $ atomically $ do
    st <- readTVar stVar
    result <- addExternsToExportEnv (exportEnv st) [ef]
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

getDbPath :: (MonadReader LspEnvironment m, MonadIO m) => m FilePath
getDbPath = do 
  env <- ask
  liftIO $ fst <$> readTVarIO (lspDbConnectionVar env)

putNewEnv :: LspEnvironment -> FilePath -> IO ()
putNewEnv env outputPath = do
  (path, newConn) <- mkConnection outputPath
  atomically $ writeTVar (lspDbConnectionVar env) (path, newConn)
  atomically $ writeTVar (lspStateVar env) emptyState