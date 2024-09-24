module Language.PureScript.Lsp.DB where

import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple.Types (Query)
import Language.PureScript.Lsp.Types (LspEnvironment (lspDbConnection))
import Protolude

queryNamed' ::
  (MonadIO m, MonadReader LspEnvironment m, FromRow r) =>
  Query ->
  [SQL.NamedParam] ->
  m [r]
queryNamed' q params = do
  conn <- asks lspDbConnection
  liftIO $ SQL.queryNamed conn q params
  
executeNamed' ::
  (MonadIO m, MonadReader LspEnvironment m) =>
  Query ->
  [SQL.NamedParam] ->
  m ()
executeNamed' q params = do
  conn <- asks lspDbConnection
  liftIO $ SQL.executeNamed conn q params
