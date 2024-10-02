{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.PureScript.Lsp.Types where

import Control.Concurrent.STM (TVar, newTVarIO)
import Database.SQLite.Simple (Connection)
import Language.PureScript.AST.Declarations qualified as P
-- import Language.PureScript.Ide.Types (IdeLogLevel)

import Language.PureScript.DB (mkConnection)
import Language.PureScript.Environment qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Names qualified as P
import Protolude
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson qualified as A

data LspEnvironment = LspEnvironment
  { lspConfig :: LspConfig,
    lspDbConnection :: Connection,
    lspStateVar :: TVar LspState
  }

mkEnv :: LspConfig -> IO LspEnvironment
mkEnv conf = do
  connection <- mkConnection $ confOutputPath conf
  st <- newTVarIO (LspState Nothing False)
  pure $ LspEnvironment conf connection st

data LspConfig = LspConfig
  { confOutputPath :: FilePath,
    confGlobs :: [FilePath]
  }
  deriving (Show)

data LspState = LspState
  { currentFile :: Maybe CurrentFile,
    lspInitalized :: Bool
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
    cidImportedDeclaration :: P.Declaration
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

decodeCompleteItemData :: Maybe A.Value -> A.Result (Maybe CompleteItemData)
decodeCompleteItemData Nothing = pure Nothing 
decodeCompleteItemData (Just v) = A.fromJSON v
