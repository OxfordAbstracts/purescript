module Language.PureScript.Lsp.Types where

import Control.Concurrent.STM (TVar)
import Database.SQLite.Simple (Connection)
import Language.PureScript qualified as P
import Language.PureScript.Ide.Types (IdeDeclarationAnn)
import Protolude

data LspEnvironment = LspEnvironment
  { lspConfig :: LspConfig,
    lspDbConnection :: Connection,
    lspStateVar :: TVar LspState
  }

data LspConfig = LspConfig
  { confOutputPath :: FilePath,
    confRootDir :: FilePath,
    confGlobs :: [FilePath]
  }
  deriving (Show)

data LspState = LspState
  { currentFile :: Maybe CurrentFile
  }
  deriving (Show)

data CurrentFile = CurrentFile
  { currentModuleName :: P.ModuleName,
    currentExternsFile :: P.ExternsFile,
    currentDeclarations :: [IdeDeclarationAnn]
  }
  deriving (Show)
