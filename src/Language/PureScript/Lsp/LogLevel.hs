module Language.PureScript.Lsp.LogLevel where


-- import Language.PureScript.Ide.Types (IdeLogLevel)

import Data.Aeson (FromJSON)
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as AT
import Protolude

data LspLogLevel
  = LogAll
  | LogDebug
  | LogPerf
  | LogInfo
  | LogWarning
  | LogError
  | LogNone
  deriving (Show, Eq, Ord, Generic)

instance A.ToJSON LspLogLevel where
  toJSON = \case
    LogAll -> A.String "all"
    LogDebug -> A.String "debug"
    LogPerf -> A.String "perf"
    LogInfo -> A.String "info"
    LogWarning -> A.String "warning"
    LogError -> A.String "error"
    LogNone -> A.String "none"

instance FromJSON LspLogLevel where
  parseJSON v = case v of
    A.String "all" -> pure LogAll
    A.String "debug" -> pure LogDebug
    A.String "perf" -> pure LogPerf
    A.String "info" -> pure LogInfo
    A.String "warning" -> pure LogWarning
    A.String "error" -> pure LogError
    A.String "none" -> pure LogNone
    A.String _ -> AT.unexpected v
    _ -> AT.typeMismatch "String" v