{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.PureScript.Lsp.Cache.Query where

-- import Language.PureScript.Bundle (getImportedModules)

import Codec.Serialise (deserialise, serialise)
import Control.Lens (Field1 (_1), (^.), _1)
import Control.Monad.Trans.Writer (execWriterT)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as Lazy
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Database.SQLite.Simple (NamedParam ((:=)), fromOnly)
import Language.LSP.Protocol.Types (Position)
import Language.PureScript.AST qualified as P
import Language.PureScript.AST.Declarations (declRefName, declSourceAnn)
import Language.PureScript.AST.Traversals (accumTypes)
import Language.PureScript.Comments qualified as P
import Language.PureScript.Externs (ExternsFile (efModuleName), externsFileName)
import Language.PureScript.Externs qualified as P
import Language.PureScript.Ide.Error (IdeError (GeneralError))
import Language.PureScript.Ide.Externs (readExternFile)
import Language.PureScript.Ide.Types (ModuleMap)
import Language.PureScript.Lsp.DB qualified as DB
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.Types (LspConfig (..), LspEnvironment (lspConfig))
import Language.PureScript.Names qualified as P
import Language.PureScript.Pretty.Types (prettyPrintType)
import Protolude
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (normalise, (</>))
import "monad-logger" Control.Monad.Logger (LoggingT, MonadLogger, logDebugN, logErrorN, logWarnN, mapLoggingT)

-- import Control.Monad.Logger (logDebugN)

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

getEfImportsMap :: (MonadIO f, MonadReader LspEnvironment f) => [P.ModuleName] -> f (Map P.ModuleName [P.DeclarationRef])
getEfImportsMap mNames = Map.fromListWith (++) . fmap (fmap List.singleton) <$> getEfExports mNames

getEfImports :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> m [P.ExternsImport]
getEfImports moduleName' = do
  imports <-
    DB.queryNamed
      "SELECT value FROM ef_imports WHERE module_name = :module_name"
      [":module_name" := P.runModuleName moduleName']
  pure $ deserialise . fromOnly <$> imports

getEfExports :: (MonadIO m, MonadReader LspEnvironment m) => [P.ModuleName] -> m [(P.ModuleName, P.DeclarationRef)]
getEfExports moduleNames = do
  exports :: [(Text, Lazy.ByteString)] <-
    DB.queryNamed
      "SELECT module_name, value FROM ef_exports WHERE module_name IN (SELECT value FROM json_each(:module_names))"
      [ ":module_names" := encode (fmap P.runModuleName moduleNames)
      ]
  pure $ bimap P.ModuleName deserialise <$> exports

importContainsIdent :: Text -> P.ExternsImport -> Maybe Bool
importContainsIdent ident import' = case P.eiImportType import' of
  P.Implicit -> Nothing
  P.Explicit refs -> Just $ any ((==) ident . printName . P.declRefName) refs
  P.Hiding refs ->
    if any ((==) ident . printName . P.declRefName) refs
      then Just False
      else Nothing

getEfDeclaration :: (MonadIO m, MonadLogger m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe (P.ModuleName, P.ExternsDeclaration))
getEfDeclaration moduleName' name = do
  inModule <- getEfDeclarationOnlyInModule moduleName' name
  case inModule of
    Just decl -> pure $ Just (moduleName', decl)
    Nothing -> getEFImportedDeclaration moduleName' name

getEFImportedDeclaration :: forall m. (MonadIO m, MonadLogger m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe (P.ModuleName, P.ExternsDeclaration))
getEFImportedDeclaration moduleName' name = do
  imports <- getEfImports moduleName'
  exported <- getEfImportsMap (fmap P.eiModule imports)
  foldM (getFromModule exported) Nothing imports
  where
    getFromModule exported acc import' = do
      case acc of
        Just _ -> pure acc
        Nothing -> case importContainsIdent name import' of
          Just False -> pure acc
          _ -> do
            inModule <- getEfDeclarationOnlyInModule importModName name
            case inModule of
              Just decl -> pure $ Just (importModName, decl)
              Nothing -> getFromExports
      where
        importModName = P.eiModule import'
        moduleExports = fromMaybe [] $ Map.lookup importModName exported

        getFromExports :: m (Maybe (P.ModuleName, P.ExternsDeclaration))
        getFromExports = foldM getFromExport Nothing moduleExports

        getFromExport ::
          Maybe (P.ModuleName, P.ExternsDeclaration) ->
          P.DeclarationRef ->
          m (Maybe (P.ModuleName, P.ExternsDeclaration))
        getFromExport acc' export' = do
          case acc of
            Just _ -> pure acc'
            Nothing -> do
              case export' of
                P.ModuleRef _ mName -> getEfDeclaration mName name
                P.ReExportRef _ss (P.ExportSource _ definedIn) ref
                  | printName (declRefName ref) == name ->
                      fmap (definedIn,) <$> getEfDeclarationOnlyInModule definedIn name
                _ -> pure acc'


getEfDeclarationOnlyInModule :: (MonadIO m, MonadLogger m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe P.ExternsDeclaration)
getEfDeclarationOnlyInModule moduleName' name = do
  decls <-
    DB.queryNamed
      "SELECT value FROM ef_declarations WHERE module_name = :module_name AND name = :name"
      [ ":module_name" := P.runModuleName moduleName',
        ":name" := name
      ]
  logDebugN $ "getEfDeclarationOnlyInModule decls: " <> show moduleName' <> " . " <> show name <> " : " <> T.pack (show $ length decls)
  pure $ deserialise . fromOnly <$> listToMaybe decls

getDeclaration :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe P.Declaration)
getDeclaration moduleName' printed_name = do
  decls <-
    DB.queryNamed
      "SELECT value FROM declarations WHERE module_name = :module_name AND printed_name = :printed_name"
      [ ":module_name" := P.runModuleName moduleName',
        ":printed_name" := printed_name
      ]
  pure $ deserialise . fromOnly <$> listToMaybe decls
