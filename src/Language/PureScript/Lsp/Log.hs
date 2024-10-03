module Language.PureScript.Lsp.Log where

import Protolude
import Language.PureScript.Lsp.Types (LspEnvironment (lspConfig), LspLogLevel (..), LspConfig (confLogLevel))



infoLsp :: (MonadIO m, MonadReader LspEnvironment m) => Text -> m ()
infoLsp = logLsp LogMsgInfo

warnLsp :: (MonadIO m, MonadReader LspEnvironment m) => Text -> m ()
warnLsp = logLsp LogMsgWarning

errorLsp  :: (MonadIO m, MonadReader LspEnvironment m) => Text -> m ()
errorLsp = logLsp LogMsgError

debugLsp :: (MonadIO m, MonadReader LspEnvironment m) => Text -> m ()
debugLsp = logLsp LogMsgDebug

perfLsp :: (MonadIO m, MonadReader LspEnvironment m) => Text -> m ()
perfLsp = logLsp LogMsgPerf


logLsp :: (MonadIO m, MonadReader LspEnvironment m) => LogMsgSeverity -> Text -> m ()
logLsp msgLogLevel msg = do
  logLevel <- confLogLevel . lspConfig <$> ask
  when (shouldLog msgLogLevel logLevel) $ do
    -- Use stderr for logging as LSP messages should be on stdout
    liftIO $ putErrLn (show msgLogLevel <> ": " <> show msg :: Text)


data LogMsgSeverity
  = LogMsgInfo
  | LogMsgWarning
  | LogMsgError
  | LogMsgDebug
  | LogMsgPerf
  deriving (Show, Eq)

shouldLog :: LogMsgSeverity -> LspLogLevel -> Bool
shouldLog msgLogLevel logLevel = case msgLogLevel of
  LogMsgInfo -> logLevel `elem` [LogInfo, LogDebug, LogAll]
  LogMsgWarning -> logLevel `elem` [LogWarning, LogInfo, LogDebug, LogAll]
  LogMsgError -> logLevel `elem` [LogError, LogWarning, LogInfo, LogDebug, LogAll]
  LogMsgDebug -> logLevel == LogDebug || logLevel == LogAll
  LogMsgPerf -> logLevel == LogPerf || logLevel == LogAll