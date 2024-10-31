{-# LANGUAGE DeriveAnyClass #-}

module Language.PureScript.Lsp.Types where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Database.SQLite.Simple (Connection, FromRow (fromRow), ToRow (toRow), field)
import Language.LSP.Protocol.Types (Range)
import Language.PureScript.DB (mkConnection)
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Names qualified as P
import Language.PureScript.Sugar.Names (Env)
import Language.PureScript.Sugar.Names qualified as P
import Protolude
import Language.PureScript.AST qualified as P
import Language.PureScript.Lsp.ServerConfig (ServerConfig, defaultConfig)
import Language.PureScript.Lsp.LogLevel (LspLogLevel)
import Codec.Serialise (deserialise, serialise)
import Language.PureScript.Lsp.NameType (LspNameType)
import Language.PureScript.TypeChecker qualified as P

data LspEnvironment = LspEnvironment
  { lspDbConnectionVar :: TVar (FilePath, Connection),
    lspStateVar :: TVar LspState,
    previousConfig :: TVar  ServerConfig
  }

mkEnv :: FilePath -> IO LspEnvironment
mkEnv outputPath = do
  connection <- newTVarIO =<< mkConnection outputPath
  st <- newTVarIO (LspState mempty P.primEnv mempty)
  prevConfig <- newTVarIO $ defaultConfig outputPath
  pure $ LspEnvironment connection st prevConfig

emptyState :: LspState
emptyState = LspState mempty P.primEnv mempty

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
    ofDependencies :: [ExternDependency],
    ofStartingEnv :: P.Environment,
    ofEndEnv :: P.Environment,
    ofEndCheckState :: P.CheckState,
    ofUncheckedModule :: P.Module,
    ofModule ::  P.Module
  }

  
data ExternDependency = ExternDependency
  { edExtern :: P.ExternsFile,
    edLevel :: Int
  } deriving (Show)

instance FromRow ExternDependency where
  fromRow = ExternDependency <$> (deserialise <$> field) <*> field

instance ToRow ExternDependency where
  toRow (ExternDependency ef level) = toRow (serialise ef, level)


data CompleteItemData = CompleteItemData
  { cidPath :: FilePath,
    cidModuleName :: P.ModuleName,
    cidImportedModuleName :: P.ModuleName,
    cidName :: Text,
    cidNameType :: LspNameType,
    cidWord :: Text,
    wordRange :: Range
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


decodeCompleteItemData :: Maybe A.Value -> A.Result (Maybe CompleteItemData)
decodeCompleteItemData Nothing = pure Nothing
decodeCompleteItemData (Just v) = A.fromJSON v
