module Language.PureScript.Lsp.DB where

import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple.Types (Query)
import Language.PureScript.Lsp.Types (LspEnvironment)
import Protolude
import Language.PureScript.Lsp.State (getDbConn)


-- initDb :: (MonadReader LspEnvironment m, MonadIO m) => FilePath -> m ()

queryNamed ::
  (MonadIO m, MonadReader LspEnvironment m, FromRow r) =>
  Query ->
  [SQL.NamedParam] ->
  m [r]
queryNamed q params = do
  conn <- getDbConn
  liftIO $ SQL.queryNamed conn q params
  
query_ ::
  (MonadIO m, MonadReader LspEnvironment m, FromRow r) =>
  Query ->
  m [r]
query_ q  = do
  conn <- getDbConn
  liftIO $ SQL.query_ conn q 
  
executeNamed ::
  (MonadIO m, MonadReader LspEnvironment m) =>
  Query ->
  [SQL.NamedParam] ->
  m ()
executeNamed q params = do
  conn <- getDbConn
  liftIO $ SQL.executeNamed conn q params

execute_ :: (MonadReader LspEnvironment m, MonadIO m) => Query -> m ()
execute_ q  = do
  conn <- getDbConn
  liftIO $ SQL.execute_ conn q 
  
