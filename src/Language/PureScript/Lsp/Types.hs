{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PackageImports #-}

module Language.PureScript.Lsp.Types where

import Control.Concurrent.STM (TVar, newTVarIO)
-- import Language.PureScript.Ide.Types (IdeLogLevel)

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Database.SQLite.Simple (Connection)
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.DB (mkConnection)
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Names qualified as P
import Protolude
import System.Directory (createDirectoryIfMissing)

data LspEnvironment = LspEnvironment
  { lspConfig :: LspConfig,
    lspDbConnection :: Connection,
    lspStateVar :: TVar LspState
  }

mkEnv :: LspConfig -> IO LspEnvironment
mkEnv conf = do
  createDirectoryIfMissing True $ confOutputPath conf
  connection <- mkConnection $ confOutputPath conf
  st <- newTVarIO (LspState Nothing)
  pure $ LspEnvironment conf connection st

data LspConfig = LspConfig
  { confOutputPath :: FilePath,
    confGlobs :: [FilePath],
    confLogLevel :: LspLogLevel
  }
  deriving (Show)

data LspState = LspState
  { currentFile :: Maybe CurrentFile
  }
  deriving (Show)

data CurrentFile = CurrentFile
  { currentModuleName :: P.ModuleName,
    currentModule :: P.Module,
    currentExternsFile :: P.ExternsFile,
    currentEnv :: P.Environment
  }
  deriving (Show)

data CompleteItemData = CompleteItemData
  { cidPath :: FilePath,
    cidModuleName :: P.ModuleName,
    cidImportedModuleName :: P.ModuleName,
    cidName :: Text,
    cidWord :: Text
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
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

decodeCompleteItemData :: Maybe A.Value -> A.Result (Maybe CompleteItemData)
decodeCompleteItemData Nothing = pure Nothing
decodeCompleteItemData (Just v) = A.fromJSON v
