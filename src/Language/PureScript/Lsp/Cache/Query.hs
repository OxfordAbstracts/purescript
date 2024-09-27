{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
import Language.PureScript.AST qualified as P
import Language.PureScript.Comments qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Names qualified as P
import Language.PureScript.AST.Declarations (declSourceAnn)
import Language.PureScript.AST.Traversals (accumTypes)
import Language.PureScript.Externs (ExternsFile (efModuleName), externsFileName)
import Language.PureScript.Ide.Error (IdeError (GeneralError))
import Language.PureScript.Ide.Externs (readExternFile)
import Language.PureScript.Ide.Types (ModuleMap)
import Language.PureScript.Lsp.DB qualified as DB
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig))
import Language.PureScript.Pretty.Types (prettyPrintType)
import Protolude
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (normalise, (</>))
import Control.Monad.Trans.Writer (execWriterT)
import Language.PureScript.Lsp.Print (printName)

-- getEfDeclarationAt :: (MonadIO m, MonadReader LspEnvironment m) => Position -> m (Maybe P.Declaration)
-- getEfDeclarationAt pos = do
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

getEfDeclaration :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe (P.ModuleName, P.ExternsDeclaration))
getEfDeclaration moduleName' name = do
  inModule <- getEfDeclarationOnlyInModule moduleName' name
  case inModule of
    Just decl -> pure $ Just (moduleName', decl)
    Nothing -> getEFImportedDeclaration moduleName' name

getEFImportedDeclaration :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe (P.ModuleName, P.ExternsDeclaration))
getEFImportedDeclaration moduleName' name = do
  imports <- filter (importMightContainIdent name) <$> getEfImports moduleName'
  foldM go Nothing imports
  where
    go ::
      (MonadIO m, MonadReader LspEnvironment m) =>
      Maybe (P.ModuleName, P.ExternsDeclaration) ->
      P.ExternsImport ->
      m (Maybe (P.ModuleName, P.ExternsDeclaration))
    go acc import' = do
      case acc of
        Just _ -> pure acc
        Nothing -> fmap (toTup $ P.eiModule import') <$> getEfDeclarationOnlyInModule (P.eiModule import') name

    toTup a b = (a, b)

getEfDeclarationOnlyInModule :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe P.ExternsDeclaration)
getEfDeclarationOnlyInModule moduleName' name = do
  decls <-
    DB.queryNamed
      "SELECT value FROM ef_declarations WHERE module_name = :module_name AND name = :name"
      [ ":module_name" := P.runModuleName moduleName',
        ":name" := name
      ]
  pure $ deserialise . fromOnly <$> listToMaybe decls

getDeclaration :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe P.Declaration)
getDeclaration moduleName' name = do
  decls <-
    DB.queryNamed
      "SELECT value FROM declarations WHERE module_name = :module_name AND name = :name"
      [ ":module_name" := P.runModuleName moduleName',
        ":name" := name
      ]
  pure $ deserialise . fromOnly <$> listToMaybe decls


getDeclarationDocumentation :: (MonadIO m, MonadReader LspEnvironment m) => P.Module -> P.Declaration -> m [P.Comment]
getDeclarationDocumentation module' decl = 
   execWriterT $ do 
    P.everywhereOnValuesM handleDecl pure pure ^. _1 $ decl
  where 
    handleDecl = pure
