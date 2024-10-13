{-# LANGUAGE DeriveAnyClass #-}

module Language.PureScript.Lsp.ServerConfig where

import Data.Aeson (FromJSON, ToJSON)
import Language.LSP.Protocol.Types (TraceValue (..))
import Language.LSP.Server (MonadLsp, getConfig, setConfig)
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (..), LspLogLevel (..))
import Protolude

data ServerConfig = ServerConfig
  { outputPath :: FilePath,
    globs :: [FilePath],
    inputSrcFromFile :: Maybe FilePath,
    logLevel :: LspLogLevel,
    traceValue :: TraceValue,
    maxTypeLength :: Maybe Int,
    maxCompletions :: Maybe Int, 
    maxFilesInCache :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

defaultFromEnv :: LspEnvironment -> ServerConfig
defaultFromEnv env =
  ServerConfig
    { outputPath = confOutputPath $ lspConfig env,
      globs = confGlobs $ lspConfig env,
      inputSrcFromFile = confInputSrcFromFile $ lspConfig env,
      logLevel = logLevel,
      traceValue = case logLevel of
        LogDebug -> TraceValue_Verbose
        LogAll -> TraceValue_Verbose
        LogWarning -> TraceValue_Messages
        _ -> TraceValue_Off,
      maxTypeLength = Nothing,
      maxCompletions = Nothing, 
      maxFilesInCache = Nothing
    }
  where
    logLevel = confLogLevel $ lspConfig env

setTraceValue :: (MonadLsp ServerConfig m) => TraceValue -> m ()
setTraceValue tv = do
  config <- getConfig
  setConfig (config {traceValue = tv})

defaultMaxTypeLength :: Int
defaultMaxTypeLength = 100

defaultMaxCompletions :: Int
defaultMaxCompletions = 50

defaultMaxFilesInCache :: Int
defaultMaxFilesInCache = 16

getMaxTypeLength :: (MonadLsp ServerConfig m) => m Int
getMaxTypeLength =
  fromMaybe defaultMaxTypeLength . maxTypeLength <$> getConfig

getMaxCompletions :: (MonadLsp ServerConfig m) => m Int
getMaxCompletions =
  fromMaybe defaultMaxCompletions . maxCompletions <$> getConfig

getMaxFilesInCache :: (MonadLsp ServerConfig m) => m Int
getMaxFilesInCache =
  fromMaybe defaultMaxFilesInCache . maxFilesInCache <$> getConfig