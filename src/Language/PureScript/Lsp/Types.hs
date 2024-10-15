{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PackageImports #-}

module Language.PureScript.Lsp.Types where

import Control.Concurrent.STM (TVar, newTVarIO)
-- import Language.PureScript.Ide.Types (IdeLogLevel)

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as AT
import Database.SQLite.Simple (Connection)
import Language.LSP.Protocol.Types (Range)
import Language.PureScript.DB (mkConnection)
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Names qualified as P
import Language.PureScript.Sugar.Names (Env)
import Language.PureScript.Sugar.Names qualified as P
import Protolude
import Language.PureScript.AST qualified as P

data LspEnvironment = LspEnvironment
  { lspDbConnection :: TVar Connection,
    lspStateVar :: TVar LspState
  }

mkEnv :: FilePath -> IO LspEnvironment
mkEnv outputPath = do
  connection <- newTVarIO =<< mkConnection outputPath
  st <- newTVarIO (LspState mempty P.primEnv mempty)
  pure $ LspEnvironment connection st

data LspConfig = LspConfig
  { confOutputPath :: FilePath,
    confGlobs :: [FilePath],
    confInputSrcFromFile :: Maybe FilePath,
    confLogLevel :: LspLogLevel
  }
  deriving (Show)

data LspState = LspState
  { openFiles :: [(FilePath, OpenFile)],
    exportEnv :: Env,
    runningRequests :: Map (Either Int32 Text) (Async ())
  }

data OpenFile = OpenFile
  { ofModuleName :: P.ModuleName,
    ofExternsFile :: P.ExternsFile,
    ofDependencies :: [P.ExternsFile],
    ofStartingEnv :: P.Environment,
    ofModule ::  P.Module
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
