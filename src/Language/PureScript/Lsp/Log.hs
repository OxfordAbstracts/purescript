module Language.PureScript.Lsp.Log where

import Data.Text qualified as T
import Data.Time (UTCTime (utctDayTime), defaultTimeLocale, formatTime, getCurrentTime)
import Language.PureScript.Lsp.Types (LspConfig (confLogLevel), LspEnvironment (lspConfig), LspLogLevel (..))
import Protolude

infoLsp :: (MonadIO m, MonadReader LspEnvironment m) => Text -> m ()
infoLsp = logLsp LogMsgInfo

warnLsp :: (MonadIO m, MonadReader LspEnvironment m) => Text -> m ()
warnLsp = logLsp LogMsgWarning

errorLsp :: (MonadIO m, MonadReader LspEnvironment m) => Text -> m ()
errorLsp = logLsp LogMsgError

debugLsp :: (MonadIO m, MonadReader LspEnvironment m) => Text -> m ()
debugLsp = logLsp LogMsgDebug

perfLsp :: (MonadIO m, MonadReader LspEnvironment m) => Text -> m ()
perfLsp = logLsp LogMsgPerf

logLsp :: (MonadIO m, MonadReader LspEnvironment m) => LogMsgSeverity -> Text -> m ()
logLsp msgLogLevel msg = do
  logLevel <- confLogLevel . lspConfig <$> ask
  when (shouldLog msgLogLevel logLevel) $ do
    now <- liftIO $ utctDayTime <$> getCurrentTime
    liftIO $
      putErrLn -- Use stderr for logging as LSP messages should be on stdout
        ( printLogMsgSeverity msgLogLevel
            <> ": "
            <> T.pack (formatTime defaultTimeLocale "%T" now)
            <> " "
            <> ": "
            <> show msg
        )

data LogMsgSeverity
  = LogMsgInfo
  | LogMsgWarning
  | LogMsgError
  | LogMsgDebug
  | LogMsgPerf
  deriving (Show, Eq)

printLogMsgSeverity :: LogMsgSeverity -> Text
printLogMsgSeverity LogMsgInfo = "INFO"
printLogMsgSeverity LogMsgWarning = "WARNING"
printLogMsgSeverity LogMsgError = "ERROR"
printLogMsgSeverity LogMsgDebug = "DEBUG"
printLogMsgSeverity LogMsgPerf = "PERF"

shouldLog :: LogMsgSeverity -> LspLogLevel -> Bool
shouldLog msgLogLevel logLevel = case msgLogLevel of
  LogMsgInfo -> logLevel `elem` [LogInfo, LogDebug, LogAll]
  LogMsgWarning -> logLevel `elem` [LogWarning, LogInfo, LogDebug, LogAll]
  LogMsgError -> logLevel `elem` [LogError, LogWarning, LogInfo, LogDebug, LogAll]
  LogMsgDebug -> logLevel == LogDebug || logLevel == LogAll
  LogMsgPerf -> logLevel == LogPerf || logLevel == LogAll