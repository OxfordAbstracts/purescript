{-# LANGUAGE DeriveAnyClass #-}

module Language.PureScript.Lsp.ServerConfig where

import Data.Aeson (FromJSON, ToJSON)
import Language.LSP.Protocol.Types (TraceValue (..))
import Language.LSP.Server (MonadLsp, getConfig, setConfig)
import Language.PureScript.Lsp.Types (LspLogLevel (..))
import Protolude

data ServerConfig = ServerConfig
  { outputPath :: FilePath,
    globs :: [FilePath],
    inputSrcFromFile :: Maybe FilePath,
    logLevel :: LspLogLevel,
    traceValue :: Maybe TraceValue,
    maxTypeLength :: Maybe Int,
    maxCompletions :: Maybe Int, 
    maxFilesInCache :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

defaultConfig :: ServerConfig
defaultConfig  =
  ServerConfig
    { outputPath = "./output",
      globs = ["./src/**/*.purs"],
      inputSrcFromFile = Nothing,
      logLevel = LogWarning,
      traceValue = Nothing,
      maxTypeLength = Just defaultMaxTypeLength,
      maxCompletions = Just defaultMaxCompletions, 
      maxFilesInCache = Just defaultMaxFilesInCache
    }

setTraceValue :: (MonadLsp ServerConfig m) => TraceValue -> m ()
setTraceValue tv = do
  config <- getConfig
  setConfig (config {traceValue = Just tv})

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