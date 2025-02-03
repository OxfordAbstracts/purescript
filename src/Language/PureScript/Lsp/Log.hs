module Language.PureScript.Lsp.Log where

import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Language.PureScript.Ide.Logging (displayTimeSpec)
import Language.PureScript.Lsp.Types (LspEnvironment)
import Protolude
import System.Clock (Clock (Monotonic), TimeSpec, diffTimeSpec, getTime)
import Language.PureScript.Lsp.ServerConfig (ServerConfig(logLevel))
import Language.LSP.Server (getConfig, MonadLsp)
import Language.PureScript.Lsp.LogLevel (LspLogLevel(..))

infoLsp :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m) => Text -> m ()
infoLsp = logLsp LogMsgInfo

warnLsp :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m) => Text -> m ()
warnLsp = logLsp LogMsgWarning

errorLsp :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m) => Text -> m ()
errorLsp = logLsp LogMsgError

debugLsp :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m) => Text -> m ()
debugLsp = logLsp LogMsgDebug

perfLsp :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m) => Text -> m ()
perfLsp = logLsp LogMsgPerf

logLsp :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m) => LogMsgSeverity -> Text -> m ()
logLsp msgLogLevel msg = do
  logLevel <- logLevel <$> getConfig
  when (shouldLog msgLogLevel logLevel) $ do
    now <- liftIO getCurrentTime
    liftIO $
      putErrLn -- Use stderr for logging as LSP messages should be on stdout
        ( "[ "
            <> printLogMsgSeverity msgLogLevel
            <> " ]"
            <> " "
            <> T.pack (formatTime defaultTimeLocale "%T" now)
            <> "\n"
            <> msg
            <> "\n\n"
        )

logPerfStandard :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m) => Text -> m t -> m t
logPerfStandard label f = logPerf (labelTimespec label) f

logPerf :: (MonadLsp ServerConfig m, MonadReader LspEnvironment m) => (TimeSpec -> Text) -> m t -> m t
logPerf format f = do
  start <- getPerfTime
  result <- f
  end <- getPerfTime
  perfLsp (format (diffTimeSpec start end))
  pure result

getPerfTime :: (MonadIO m) => m TimeSpec
getPerfTime = liftIO (getTime Monotonic)

labelTimespec :: Text -> TimeSpec -> Text
labelTimespec label duration = label <> ": " <> displayTimeSpec duration

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