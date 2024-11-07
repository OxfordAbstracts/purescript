{-# LANGUAGE DeriveAnyClass #-}

module Language.PureScript.Lsp.Types where

import Codec.Serialise (deserialise, serialise)
import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Database.SQLite.Simple (Connection, FromRow (fromRow), ToRow (toRow), field)
import Language.LSP.Protocol.Types (Range)
import Language.PureScript.AST qualified as P
import Language.PureScript.DB (mkConnection)
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Lsp.LogLevel (LspLogLevel)
import Language.PureScript.Lsp.NameType (LspNameType)
import Language.PureScript.Lsp.ServerConfig (ServerConfig, defaultConfig)
import Language.PureScript.Names qualified as P
import Language.PureScript.Sugar.Names (Env)
import Language.PureScript.Sugar.Names qualified as P
import Protolude
import Language.PureScript.TypeChecker.IdeArtifacts (IdeArtifacts)

data LspEnvironment = LspEnvironment
  { lspDbConnectionVar :: TVar (FilePath, Connection),
    lspStateVar :: TVar LspState,
    previousConfig :: TVar ServerConfig
  }

mkEnv :: FilePath -> IO LspEnvironment
mkEnv outputPath = do
  connection <- newTVarIO =<< mkConnection outputPath
  st <- newTVarIO emptyState
  prevConfig <- newTVarIO $ defaultConfig outputPath
  pure $ LspEnvironment connection st prevConfig

emptyState :: LspState
emptyState = LspState mempty P.primEnv mempty mempty

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
    environments :: [((FilePath, Int), (P.Env, P.Environment))],
    runningRequests :: Map (Either Int32 Text) (Async ())
  }

data OpenFile = OpenFile
  { ofModuleName :: P.ModuleName,
    ofExternsFile :: P.ExternsFile,
    ofArtifacts :: IdeArtifacts,
    ofModule :: P.Module
  }

data ExternDependency = ExternDependency
  { edExtern :: P.ExternsFile,
    edLevel :: Int,
    edHash :: Int
  }
  deriving (Show)

instance FromRow ExternDependency where
  fromRow = ExternDependency <$> (deserialise <$> field) <*> field <*> field

instance ToRow ExternDependency where
  toRow (ExternDependency ef level updated_at) = toRow (serialise ef, level, updated_at)

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
