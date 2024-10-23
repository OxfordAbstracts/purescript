{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.PureScript.Lsp.Cache.Query where

import Database.SQLite.Simple (NamedParam ((:=)), fromOnly)
import Database.SQLite.Simple qualified as SQL
import Language.LSP.Server (MonadLsp)
import Language.PureScript.AST qualified as P
import Language.PureScript.AST.SourcePos (SourcePos (SourcePos))
import Language.PureScript.Lsp.DB qualified as DB
import Language.PureScript.Lsp.NameType (LspNameType)
import Language.PureScript.Lsp.ServerConfig (ServerConfig, getMaxCompletions, getMaxTypeLength)
import Language.PureScript.Lsp.Types (LspEnvironment)
import Language.PureScript.Names qualified as P
import Protolude
import Language.PureScript.Lsp.Log (debugLsp)

------------------------------------------------------------------------------------------------------------------------
------------ AST -------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

getAstDeclarationInModule :: (MonadIO m, MonadReader LspEnvironment m) => P.ModuleName -> Text -> LspNameType -> m (Maybe (Text, Maybe Text))
getAstDeclarationInModule moduleName' name nameType = do
  decls <-
    DB.queryNamed
      "SELECT name, ctr_type FROM ast_declarations WHERE module_name = :module_name AND name = :name AND name_type IS :name_type"
      [ ":module_name" := P.runModuleName moduleName',
        ":name" := name,
        ":name_type" := nameType
      ]

  pure $ listToMaybe decls

getAstDeclarationLocationInModule :: (MonadIO m, MonadReader LspEnvironment m) =>  LspNameType -> P.ModuleName -> Text -> m [P.SourceSpan]
getAstDeclarationLocationInModule lspNameType moduleName' name = do
  decls :: [([Char], Int, Int, Int, Int)] <-
    DB.queryNamed
      "SELECT path, start_line, start_col, end_line, end_col \
      \FROM ast_declarations \
      \INNER JOIN ast_modules on ast_declarations.module_name = ast_modules.module_name \
      \WHERE ast_declarations.module_name = :module_name \
      \AND name = :name \
      \AND name_type IS :name_type"
      [ ":module_name" := P.runModuleName moduleName',
        ":name" := name,
        ":name_type" := lspNameType
      ]
  pure $ decls <&> \(spanName, sl, sc, el, ec) -> P.SourceSpan spanName (SourcePos sl sc) (SourcePos el ec)

getAstDeclarationTypeInModule :: (MonadIO m, MonadReader LspEnvironment m) => Maybe LspNameType -> P.ModuleName -> Text -> m [Text]
getAstDeclarationTypeInModule lspNameType moduleName' name = do
  decls :: [SQL.Only Text] <-
    DB.queryNamed
      "SELECT printed_type \
      \FROM ast_declarations \
      \INNER JOIN ast_modules on ast_declarations.module_name = ast_modules.module_name \
      \WHERE ast_declarations.module_name = :module_name \
      \AND name = :name \
      \AND name_type IS :name_type"
      [ ":module_name" := P.runModuleName moduleName',
        ":name" := name,
        ":name_type" := lspNameType
      ]
  pure $ decls <&> fromOnly

getAstDeclarationsStartingWith ::
  (MonadIO m, MonadReader LspEnvironment m, MonadLsp ServerConfig m) =>
  P.ModuleName ->
  Text ->
  m [CompletionResult]
getAstDeclarationsStartingWith moduleName' prefix = do
  debugLsp $ "prefix: " <> prefix
  limit <- getMaxCompletions
  typeLen <- getMaxTypeLength
  let offset = 0 :: Int
  DB.queryNamed
    ( SQL.Query $
        "SELECT ast_declarations.name, "
          <> printedTypeTruncated typeLen
          <> "ast_declarations.module_name, ast_declarations.name_type FROM ast_declarations \
             \INNER JOIN ast_modules on ast_declarations.module_name = ast_modules.module_name \
             \INNER JOIN available_srcs on ast_modules.path = available_srcs.path \
             \WHERE (ast_declarations.module_name = :module_name OR ast_declarations.exported) \
             \AND instr(name, :prefix) == 1 \
             \AND generated = false \
             \ORDER BY name ASC \
             \LIMIT :limit \
             \OFFSET :offset"
    )
    [ ":module_name" := P.runModuleName moduleName',
      ":prefix" := prefix,
      ":limit" := limit,
      ":offset" := offset
    ]

getAstDeclarationsStartingWithAndSearchingModuleNames ::
  (MonadIO m, MonadReader LspEnvironment m, MonadLsp ServerConfig m) =>
  P.ModuleName ->
  P.ModuleName ->
  Text ->
  m [CompletionResult]
getAstDeclarationsStartingWithAndSearchingModuleNames moduleName' moduleNameContains prefix = do
  limit <- getMaxCompletions
  typeLen <- getMaxTypeLength
  let offset = 0 :: Int
  DB.queryNamed
    ( SQL.Query $
        "SELECT ast_declarations.name, "
          <> printedTypeTruncated typeLen
          <> "ast_declarations.module_name, ast_declarations.name_type FROM ast_declarations \
             \INNER JOIN ast_modules on ast_declarations.module_name = ast_modules.module_name \
             \INNER JOIN available_srcs on ast_modules.path = available_srcs.path \
             \WHERE (ast_declarations.module_name = :module_name OR ast_declarations.exported) \
             \AND instr(ast_declarations.module_name, :module_name_contains) <> 0 \
             \AND instr(name, :prefix) == 1 \
             \AND generated = false \
             \ORDER BY name ASC \
             \LIMIT :limit \
             \OFFSET :offset"
    )
    [ ":module_name" := P.runModuleName moduleName',
      ":prefix" := prefix,
      ":module_name_contains" := P.runModuleName moduleNameContains,
      ":limit" := limit,
      ":offset" := offset
    ]

getAstDeclarationsStartingWithOnlyInModule ::
  (MonadIO m, MonadReader LspEnvironment m, MonadLsp ServerConfig m) =>
  P.ModuleName ->
  Text ->
  m [CompletionResult]
getAstDeclarationsStartingWithOnlyInModule moduleName' prefix = do
  limit <- getMaxCompletions
  typeLen <- getMaxTypeLength
  let offset = 0 :: Int
  DB.queryNamed
    ( SQL.Query $
        "SELECT ast_declarations.name, "
          <> printedTypeTruncated typeLen
          <> "ast_declarations.module_name, ast_declarations.name_type FROM ast_declarations \
             \INNER JOIN ast_modules on ast_declarations.module_name = ast_modules.module_name \
             \INNER JOIN available_srcs on ast_modules.path = available_srcs.path \
             \WHERE ast_declarations.module_name = :module_name \
             \AND instr(name, :prefix) == 1 \
             \AND generated = false \
             \ORDER BY name ASC \
             \LIMIT :limit \
             \OFFSET :offset"
    )
    [ ":module_name" := P.runModuleName moduleName',
      ":prefix" := prefix,
      ":limit" := limit,
      ":offset" := offset
    ]

printedTypeTruncated :: Int -> Text
printedTypeTruncated typeLen =
  " CASE \
  \WHEN LENGTH (ast_declarations.printed_type) > "
    <> show typeLen
    <> " THEN substr (ast_declarations.printed_type, 1, "
    <> show (typeLen `div` 2)
    <> ") || '...' "
    <> " || substr (ast_declarations.printed_type, -"
    <> show (typeLen `div` 2)
    <> ") \
       \ELSE ast_declarations.printed_type \
       \END printed_type, "

data CompletionResult = CompletionResult
  { crName :: Text,
    crType :: Text,
    crModule :: P.ModuleName,
    crNameType :: LspNameType
  }
  deriving (Show, Generic)

instance SQL.FromRow CompletionResult where
  fromRow = CompletionResult <$> SQL.field <*> SQL.field <*> (P.ModuleName <$> SQL.field) <*> SQL.field 