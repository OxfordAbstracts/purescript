module Language.PureScript.Lsp.DB where

import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple.Types (Query)
import Language.PureScript.Lsp.Types (LspEnvironment (lspDbConnection))
import Protolude


-- initDb :: (MonadReader LspEnvironment m, MonadIO m) => FilePath -> m ()

queryNamed ::
  (MonadIO m, MonadReader LspEnvironment m, FromRow r) =>
  Query ->
  [SQL.NamedParam] ->
  m [r]
queryNamed q params = do
  conn <- asks lspDbConnection
  liftIO $ SQL.queryNamed conn q params
  
query_ ::
  (MonadIO m, MonadReader LspEnvironment m, FromRow r) =>
  Query ->
  m [r]
query_ q  = do
  conn <- asks lspDbConnection
  liftIO $ SQL.query_ conn q 
  
executeNamed ::
  (MonadIO m, MonadReader LspEnvironment m) =>
  Query ->
  [SQL.NamedParam] ->
  m ()
executeNamed q params = do
  conn <- asks lspDbConnection
  liftIO $ SQL.executeNamed conn q params

execute_ :: (MonadReader LspEnvironment m, MonadIO m) => Query -> m ()
execute_ q  = do
  conn <- asks lspDbConnection
  liftIO $ SQL.execute_ conn q 
  
