{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.PureScript.Lsp.Cache.Query where

-- import Language.PureScript.Bundle (getImportedModules)

import Codec.Serialise (deserialise, serialise)
import Control.Lens (Field1 (_1), (^.), _1)
import Control.Monad.Trans.Writer (execWriterT)
import Data.Aeson (encode)
import Data.Aeson.Types qualified as A
import Data.ByteString.Lazy qualified as Lazy
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text qualified as T
import Database.SQLite.Simple (NamedParam ((:=)), fromOnly)
import Database.SQLite.Simple qualified as SQL
import GHC.Base (String)
import GHC.Real (Integral (toInteger))
import Language.LSP.Protocol.Types (Position)
import Language.LSP.Protocol.Types qualified as LSP
import Language.PureScript.AST qualified as P
import Language.PureScript.AST.Declarations (declRefName, declSourceAnn)
import Language.PureScript.AST.Traversals (accumTypes)
import Language.PureScript.Comments qualified as P
import Language.PureScript.CoreFn qualified as CF
import Language.PureScript.CoreFn.Expr as CF
import Language.PureScript.CoreFn.FromJSON qualified as CF
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
import Protolude qualified as Either
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (normalise, (</>))
import "monad-logger" Control.Monad.Logger (LoggingT, MonadLogger, logDebugN, logErrorN, logWarnN, mapLoggingT)
import Language.PureScript (Ident)
import Data.Aeson qualified as A

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

getCoreFnExprAt :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> LSP.Position -> m (Maybe (CF.Expr CF.Ann))
getCoreFnExprAt path (LSP.Position line col) = do
  decls :: [SQL.Only Lazy.ByteString] <-
    DB.queryNamed
      "SELECT corefn_expressions.value FROM corefn_expressions \
      \INNER JOIN corefn_modules on corefn_expressions.module_name = corefn_modules.name \
      \WHERE start_line <= :line AND end_line >= :line \
      \AND start_col <= :column AND end_col >= :column \
      \AND path = :path \
      \AND lines = 0 \
      \ORDER BY cols ASC \
      \LIMIT 1"
      [ ":line" := toInteger (line + 1),
        ":column" := toInteger (col + 1),
        ":path" := path
      ]

  pure $
    A.parseMaybe (CF.exprFromJSON path)
      =<< A.decode'
      =<< fromOnly
      <$> listToMaybe decls

getCodeFnBindAt :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> LSP.Position -> m (Maybe (CF.Bind CF.Ann))
getCodeFnBindAt path (LSP.Position line col) = do
  decls :: [SQL.Only Lazy.ByteString] <-
    DB.queryNamed
      "SELECT corefn_declarations.value FROM corefn_declarations \
      \INNER JOIN corefn_modules on corefn_declarations.module_name = corefn_modules.name \
      \WHERE start_line <= :line AND end_line >= :line \
      \AND start_col <= :column AND end_col >= :column \
      \AND path = :path \
      \AND lines = 0 \
      \ORDER BY cols ASC \
      \LIMIT 1"
      [ ":line" := toInteger (line + 1),
        ":column" := toInteger (col + 1),
        ":path" := path
      ]
  pure $
    A.parseMaybe (CF.bindFromJSON path)
      =<< A.decode'
      =<< fromOnly
      <$> listToMaybe decls

-- findLocalBinding :: (P.Ident -> Bool) -> Expr a -> Maybe (CF.Binder a)
-- findLocalBinding f = go
--   where
--     go (Abs _ ident _) | f ident = Just (VarBinder nullSourceAnn ident)
--     go (Let _ binds _) = asum (fmap (go . binder) binds)
--     go _ = Nothing

------------------------------------------------------------------------------------------------------------------------
------------ Externs ---------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

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

