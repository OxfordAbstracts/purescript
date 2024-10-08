{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PackageImports #-}

module Language.PureScript.Lsp.Types where

import Control.Concurrent.STM (TVar, newTVarIO)
-- import Language.PureScript.Ide.Types (IdeLogLevel)

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Database.SQLite.Simple (Connection)
import Language.LSP.Protocol.Types (Range)
import Language.PureScript.DB (mkConnection)
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Names qualified as P
import Protolude
import System.Directory (createDirectoryIfMissing)
import Data.Aeson.Types qualified as AT

data LspEnvironment = LspEnvironment
  { lspConfig :: LspConfig,
    lspDbConnection :: Connection,
    lspStateVar :: TVar LspState
  }

mkEnv :: LspConfig -> IO LspEnvironment
mkEnv conf = do
  createDirectoryIfMissing True $ confOutputPath conf
  connection <- mkConnection $ confOutputPath conf
  st <- newTVarIO (LspState mempty mempty)
  pure $ LspEnvironment conf connection st

data LspConfig = LspConfig
  { confOutputPath :: FilePath,
    confGlobs :: [FilePath],
    confInputSrcFromFile :: Maybe FilePath,
    confLogLevel :: LspLogLevel
  }
  deriving (Show)

data LspState = LspState
  { openFiles :: Map FilePath OpenFile,
    cancelledRequests :: Set (Either Int32 Text)
  }
  deriving (Show)

data OpenFile = OpenFile
  { ofModuleName :: P.ModuleName,
    ofExternsFile :: P.ExternsFile,
    ofEnv :: P.Environment
  }
  deriving (Show)

data CompleteItemData = CompleteItemData
  { cidPath :: FilePath,
    cidModuleName :: P.ModuleName,
    cidImportedModuleName :: P.ModuleName,
    cidName :: Text,
    cidWord :: Text,
    wordRange :: Range
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

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

decodeCompleteItemData :: Maybe A.Value -> A.Result (Maybe CompleteItemData)
decodeCompleteItemData Nothing = pure Nothing
decodeCompleteItemData (Just v) = A.fromJSON v
