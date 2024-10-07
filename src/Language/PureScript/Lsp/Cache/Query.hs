{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.PureScript.Lsp.Cache.Query where

import Codec.Serialise (deserialise)
import Data.Aeson (encode)
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.ByteString.Lazy qualified as Lazy
import Data.List qualified as List
import Data.Map qualified as Map
import Database.SQLite.Simple (NamedParam ((:=)), fromOnly)
import Database.SQLite.Simple qualified as SQL
import Language.LSP.Protocol.Types qualified as LSP
import Language.PureScript.AST qualified as P
import Language.PureScript.AST.SourcePos (SourcePos (SourcePos))
import Language.PureScript.CoreFn qualified as CF
import Language.PureScript.CoreFn.Expr as CF
import Language.PureScript.CoreFn.FromJSON qualified as CF
import Language.PureScript.Externs qualified as P
import Language.PureScript.Lsp.DB qualified as DB
import Language.PureScript.Lsp.Types (LspEnvironment)
import Language.PureScript.Names qualified as P
import Protolude

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

getEfDeclarationInModule :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe P.ExternsDeclaration)
getEfDeclarationInModule moduleName' name = do
  decls <-
    DB.queryNamed
      "SELECT value FROM ef_declarations WHERE module_name = :module_name AND name = :name"
      [ ":module_name" := P.runModuleName moduleName',
        ":name" := name
      ]
  pure $ deserialise . fromOnly <$> listToMaybe decls

getEfDeclarationsAtSrcPos :: (MonadIO m, MonadReader LspEnvironment m) => FilePath -> SourcePos -> m [P.ExternsDeclaration]
getEfDeclarationsAtSrcPos path (SourcePos line col) = do
  decls <-
    DB.queryNamed
      "SELECT ef_declarations.value FROM ef_declarations \
      \inner join externs on ef_declarations.module_name = externs.module_name \
      \WHERE start_line <= :line AND end_line >= :line \
      \AND start_col <= :column AND end_col >= :column \
      \AND path = :path"
      [ ":line" := line,
        ":column" := col,
        ":path" := path
      ]
  pure $ deserialise . fromOnly <$> decls

getAstDeclarationInModule :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> m (Maybe P.Declaration)
getAstDeclarationInModule moduleName' name = do
  decls <-
    DB.queryNamed
      "SELECT value FROM ast_declarations WHERE module_name = :module_name AND name = :name"
      [ ":module_name" := P.runModuleName moduleName',
        ":name" := name
      ]
  pure $ deserialise . fromOnly <$> listToMaybe decls

getAstDeclarationsAtSrcPos :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> SourcePos -> m [P.Declaration]
getAstDeclarationsAtSrcPos moduleName' (SourcePos line col) = do
  decls <-
    DB.queryNamed
      "SELECT value FROM ast_declarations \
      \WHERE start_line <= :line AND end_line >= :line \
      \AND start_col <= :column AND end_col >= :column \
      \AND module_name = :module_name \
      \ORDER BY lines ASC, cols ASC"
      [ ":line" := line,
        ":column" := col,
        ":module_name" := P.runModuleName moduleName'
      ]
  pure $ deserialise . fromOnly <$> decls

getAstDeclarationsStartingWith ::
  (MonadIO m, MonadReader LspEnvironment m) =>
  Int ->
  Int ->
  P.ModuleName ->
  Text ->
  m [CompletionResult]
getAstDeclarationsStartingWith limit offset moduleName' prefix = do
  DB.queryNamed
    "SELECT ast_declarations.name, ast_declarations.printed_type, ast_declarations.module_name FROM ast_declarations \
    \INNER JOIN ast_modules on ast_declarations.module_name = ast_modules.name \
    \INNER JOIN available_srcs on ast_modules.path = available_srcs.path \
    \WHERE (module_name = :module_name OR exported) \
    \AND name GLOB :prefix \
    \ORDER BY name ASC \
    \LIMIT :limit \
    \OFFSET :offset"
    [ ":module_name" := P.runModuleName moduleName',
      ":prefix" := prefix <> "*",
      ":limit" := limit,
      ":offset" := offset
    ]

data CompletionResult = CompletionResult
  { crName :: Text,
    crType :: Text,
    crModule :: P.ModuleName
  }
  deriving (Show, Generic)

instance SQL.FromRow CompletionResult where
  fromRow = CompletionResult <$> SQL.field <*> SQL.field <*> (P.ModuleName <$> SQL.field)