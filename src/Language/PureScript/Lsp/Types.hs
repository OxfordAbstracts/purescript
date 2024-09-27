{-# LANGUAGE PackageImports #-}

module Language.PureScript.Lsp.Types where

import Control.Concurrent.STM (TVar, newTVarIO)
import Database.SQLite.Simple (Connection, open)
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Names qualified as P
-- import Language.PureScript.Ide.Types (IdeLogLevel)
import Protolude

data LspEnvironment = LspEnvironment
  { lspConfig :: LspConfig,
    lspDbConnection :: Connection,
    lspStateVar :: TVar LspState
  }

mkEnv :: LspConfig -> IO LspEnvironment
mkEnv conf = do 
   connection <- open (confOutputPath conf <> "lsp.sqlite") 
   st <- newTVarIO (LspState Nothing False)
   pure $ LspEnvironment conf connection st

data LspConfig = LspConfig
  { confOutputPath :: FilePath,
    confGlobs :: [FilePath]
  }
  deriving (Show)

data LspState = LspState
  { currentFile :: Maybe CurrentFile
  , lspInitalized :: Bool
  }
  deriving (Show)

data CurrentFile = CurrentFile
  { currentModuleName :: P.ModuleName,
    currentModule :: P.Module,
    currentExternsFile :: P.ExternsFile
    
  }
  deriving (Show)
