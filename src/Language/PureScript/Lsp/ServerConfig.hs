{-# LANGUAGE DeriveAnyClass #-}

module Language.PureScript.Lsp.ServerConfig where

import Data.Aeson (FromJSON, ToJSON)
import Language.LSP.Protocol.Types (TraceValue (..))
import Language.LSP.Server (MonadLsp, getConfig, setConfig)
import Language.PureScript.Lsp.LogLevel (LspLogLevel (..))
import Protolude

data ServerConfig = ServerConfig
  { outputPath :: FilePath,
    globs :: [FilePath],
    inputSrcFromFile :: Maybe FilePath,
    logLevel :: LspLogLevel,
    traceValue :: Maybe TraceValue,
    maxTypeLength :: Maybe Int,
    maxCompletions :: Maybe Int, 
    maxFilesInCache :: Maybe Int, 
    inferExpressions :: Bool,
    showDiagnosticsModule :: Bool,
    showDiagnosticsFilepath :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

defaultConfig :: FilePath -> ServerConfig
defaultConfig outputPath =
  ServerConfig
    { outputPath = outputPath,
      globs = ["./src/**/*.purs"],
      inputSrcFromFile = Nothing,
      logLevel = LogAll,
      traceValue = Nothing,
      maxTypeLength = Just defaultMaxTypeLength,
      maxCompletions = Just defaultMaxCompletions, 
      maxFilesInCache = Just defaultMaxFilesInCache,
      inferExpressions = True,
      showDiagnosticsModule = False,
      showDiagnosticsFilepath = False
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
defaultMaxFilesInCache = 32

getMaxTypeLength :: (MonadLsp ServerConfig m) => m Int
getMaxTypeLength =
  fromMaybe defaultMaxTypeLength . maxTypeLength <$> getConfig

getMaxCompletions :: (MonadLsp ServerConfig m) => m Int
getMaxCompletions =
  fromMaybe defaultMaxCompletions . maxCompletions <$> getConfig

getMaxFilesInCache :: (MonadLsp ServerConfig m) => m Int
getMaxFilesInCache =
  fromMaybe defaultMaxFilesInCache . maxFilesInCache <$> getConfig


getInferExpressions :: (MonadLsp ServerConfig m) => m Bool
getInferExpressions = inferExpressions <$> getConfig  