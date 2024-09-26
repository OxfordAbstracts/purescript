{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.PureScript.Lsp.Cache.Query where

-- import Language.PureScript.Bundle (getImportedModules)

import Codec.Serialise (deserialise, serialise)
import Control.Lens (Field1 (_1), (^.), _1)
import Data.Aeson (encode)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple
import Language.LSP.Protocol.Types (Position)
import Language.PureScript qualified as P
import Language.PureScript.AST.Declarations (declSourceAnn)
import Language.PureScript.AST.Traversals (accumTypes)
import Language.PureScript.Externs (ExternsFile (efModuleName), externsFileName)
import Language.PureScript.Ide.Error (IdeError (GeneralError))
import Language.PureScript.Ide.Externs (readExternFile)
import Language.PureScript.Ide.Types (ModuleMap)
import Language.PureScript.Lsp.DB qualified as DB
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig))
import Language.PureScript.Lsp.Util (printName)
import Language.PureScript.Pretty.Types (prettyPrintType)
import Protolude
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (normalise, (</>))

-- getDeclarationAt :: (MonadIO m, MonadReader LspEnvironment m) => Position -> m (Maybe P.Declaration)
-- getDeclarationAt pos = do
--   decls <-
--     DB.queryNamed
--       "SELECT * FROM declarations WHERE startLine <= :line AND endLine >= :line AND startColumn <= :column AND endColumn >= :column"
--       [":line" := line
--       , ":column" := column
--       ]
--   pure $ listToMaybe decls

-- getImportedModules


getEfImports :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> m [P.ExternsImport]
getEfImports moduleName' = do
  imports <-
    DB.queryNamed
      "SELECT value FROM ef_imports WHERE module_name = :module_name"
      [":module_name" := P.runModuleName moduleName']
  pure $ deserialise . fromOnly <$> imports

importMightContainIdent :: Text -> P.ExternsImport -> Bool
importMightContainIdent ident import' = case P.eiImportType import' of
  P.Implicit -> True
  P.Explicit refs -> any ((==) ident . printName . P.declRefName) refs
  P.Hiding refs -> not $ any ((==) ident . printName . P.declRefName) refs

getDeclaration :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe P.ExternsDeclaration)
getDeclaration moduleName' name = do 
  inModule <- getEfDeclarationOnlyInModule moduleName' name
  case inModule of
    Just decl -> pure $ Just decl
    Nothing -> getImportedDeclaration moduleName' name


getImportedDeclaration :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe P.ExternsDeclaration)
getImportedDeclaration moduleName' name = do
  imports <- filter (importMightContainIdent name) <$> getEfImports moduleName'
  foldM go Nothing imports
  where
    go :: (MonadIO m, MonadReader LspEnvironment m) => Maybe P.ExternsDeclaration -> P.ExternsImport -> m (Maybe P.ExternsDeclaration)
    go acc import' = do
      case acc of
        Just _ -> pure acc
        Nothing -> getEfDeclarationOnlyInModule (P.eiModule import') name

getEfDeclarationOnlyInModule :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe P.ExternsDeclaration)
getEfDeclarationOnlyInModule moduleName' name = do
  decls <-
    DB.queryNamed
      "SELECT value FROM ef_declarations WHERE module_name = :module_name AND name = :name"
      [ ":module_name" := P.runModuleName moduleName',
        ":name" := name
      ]
  pure $ deserialise . fromOnly <$> listToMaybe decls
