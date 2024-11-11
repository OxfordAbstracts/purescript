{-# LANGUAGE TypeOperators #-}

module Language.PureScript.Lsp.State
  ( getState, 
    getDbConn,
    cacheRebuild,
    cacheRebuild',
    updateCachedModule,
    updateCachedModule',
    cachedRebuild,
    clearCache,
    clearEnvCache,
    clearRebuildCache,
    clearExportCache,
    mergeExportEnvCache,
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
    putPreviousConfig,
    getPreviousConfig,
    cachedFiles,
    cachedFilePaths,
    cachedEnvironment,
    cacheEnvironment,
    hashDeps,
    hashDepHashs,
    cachedOpenFileFromSrc,
    updateCachedRebuildResult,
    -- cachedExportEnvironment,
    -- cacheExportEnvironment,
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
import Language.PureScript.Lsp.Log (errorLsp)
import Language.PureScript.Lsp.ServerConfig (ServerConfig, getMaxFilesInCache)
import Language.PureScript.Lsp.Types
import Language.PureScript.Sugar.Names (externsEnv)
import Language.PureScript.Sugar.Names.Env qualified as P
import Protolude hiding (moduleName, unzip)
import Language.PureScript.TypeChecker.IdeArtifacts (IdeArtifacts)

getDbConn :: (MonadReader LspEnvironment m, MonadIO m) => m Connection
getDbConn = liftIO . fmap snd . readTVarIO . lspDbConnectionVar =<< ask


getState :: (MonadReader LspEnvironment m, MonadIO m) => m LspState
getState = liftIO . readTVarIO . lspStateVar =<< ask

-- | Sets rebuild cache to the given ExternsFile
cacheRebuild :: (MonadReader LspEnvironment m, MonadLsp ServerConfig m) => Text -> ExternsFile -> IdeArtifacts -> P.Module -> Int -> m ()
cacheRebuild src ef artifacts module' depHash = do
  st <- lspStateVar <$> ask
  maxFiles <- getMaxFilesInCache
  liftIO $ cacheRebuild' st maxFiles src ef artifacts module' depHash

cacheRebuild' :: TVar LspState -> Int -> Text -> ExternsFile -> IdeArtifacts -> P.Module -> Int -> IO ()
cacheRebuild' st maxFiles src ef artifacts module' depHash = atomically . modifyTVar st $ \x ->
  x
    { openFiles = List.take maxFiles $ (fp, OpenFile (efModuleName ef) src ef artifacts  module' depHash Nothing) : filter ((/= fp) . fst) (openFiles x)
    }
  where
    fp = P.spanName $ efSourceSpan ef

updateCachedModule :: (MonadIO m, MonadReader LspEnvironment m) => P.Module -> m ()
updateCachedModule module' = do
  st <- lspStateVar <$> ask
  updateCachedModule' st module'

updateCachedModule' :: (MonadIO m) => TVar LspState -> P.Module -> m ()
updateCachedModule' st module' = liftIO . atomically $ modifyTVar st $ \x ->
  x
    { openFiles =
        openFiles x <&> \(fp, ofile) ->
          if ofModuleName ofile == P.getModuleName module'
            then (fp, ofile {ofModule = module'})
            else (fp, ofile)
    }

updateCachedRebuildResult :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> RebuildResult -> m ()
updateCachedRebuildResult fp result = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ modifyTVar st $ \x ->
    x
      { openFiles =
          openFiles x <&> \(fp', ofile) ->
            if fp == fp'
              then (fp', ofile {ofRebuildResult = Just result})
              else (fp', ofile)
      }

cachedOpenFileFromSrc :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> Text  -> m (Maybe OpenFile)
cachedOpenFileFromSrc fp input = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ do
    st' <- readTVar st
    pure $ snd <$> List.find (\(fp', ofile) -> fp == fp' && input == ofSrc ofile) (openFiles st')

cachedRebuild :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> m (Maybe OpenFile)
cachedRebuild fp = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ do
    st' <- readTVar st
    pure $ List.lookup fp $ openFiles st'

cachedFiles :: (MonadIO m, MonadReader LspEnvironment m) => m [(FilePath, OpenFile)]
cachedFiles = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ openFiles <$> readTVar st

cachedFilePaths :: (MonadIO m, MonadReader LspEnvironment m) => m [FilePath]
cachedFilePaths = fmap fst <$> cachedFiles

cacheEnvironment :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m) => FilePath -> [ExternDependency] -> P.Env -> P.Environment -> m ()
cacheEnvironment fp deps exportEnv env = do
  st <- lspStateVar <$> ask
  maxFiles <- getMaxFilesInCache
  liftIO . atomically $ modifyTVar st $ \x ->
    x
      { environments = take maxFiles $ ((fp, hashDeps deps), (exportEnv, env)) : filter ((/= fp) . fst . fst) (environments x)
      }

-- use the cache environment functions for rebuilding 
-- remove unneeded stuff from open files 
-- look into persiting envs when client is idle (on vscode client)
-- update default open files in client 

cachedEnvironment :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> [ExternDependency] -> m (Maybe (P.Env, P.Environment))
cachedEnvironment fp deps = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ do
    fmap snd .  find match . environments <$> readTVar st

  where
    hashed = hashDeps deps
    match ((fp', hash'), _) = fp == fp' && hash' == hashed


hashDeps :: [ExternDependency] -> Int
hashDeps = hashDepHashs . fmap edHash

hashDepHashs :: [Int] -> Int
hashDepHashs = hash . sort

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

clearEnvCache :: (MonadReader LspEnvironment m, MonadIO m) => m ()
clearEnvCache = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ modifyTVar st $ \x -> x {environments = []}

clearExportCache :: (MonadReader LspEnvironment m, MonadIO m) => m ()
clearExportCache = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ modifyTVar st $ \x -> x {exportEnv = P.primEnv}

clearCache :: (MonadReader LspEnvironment m, MonadIO m) => m ()
clearCache = clearRebuildCache >> clearEnvCache >> clearExportCache

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


mergeExportEnvCache :: (MonadIO m, MonadReader LspEnvironment m) => P.Env -> m ()
mergeExportEnvCache env = do
  st <- lspStateVar <$> ask
  liftIO . atomically $ modifyTVar st $ \x -> x {exportEnv = env}

data BuildEnvCacheException = BuildEnvCacheException Text
  deriving (Show)

instance Exception BuildEnvCacheException

addExternsToExportEnv :: (Foldable t, Monad m) => P.Env -> t ExternsFile -> m (Either MultipleErrors P.Env)
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

getPreviousConfig :: (MonadReader LspEnvironment m, MonadIO m) => m ServerConfig
getPreviousConfig = liftIO . readTVarIO . previousConfig =<< ask

putPreviousConfig :: (MonadReader LspEnvironment m, MonadIO m) => ServerConfig -> m ()
putPreviousConfig config = liftIO . atomically . flip writeTVar config . previousConfig =<< ask