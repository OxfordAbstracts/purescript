{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Monads for persisting the type checker environment.
module Language.PureScript.TypeChecker.Persist where

import Control.Arrow ((>>>))
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently_)
import Control.Monad.Writer.Class (MonadWriter (..))
import Data.Map qualified as Map
import Database.SQLite.Simple (Connection, Only (Only, fromOnly), execute, query, execute_)
import Language.PureScript.Environment (DataDeclType, Environment (..), NameKind (..), NameVisibility (..), TypeClassData (..), TypeKind (..))
import Language.PureScript.Environment qualified as E
import Language.PureScript.Names (Ident (..), ProperName (..), ProperNameType (..), Qualified (..))
import Language.PureScript.Types (SourceType)
import Protolude
import Codec.Serialise (serialise, deserialise)

class PersistEnvM m where
  initDb :: m ()
  persistEnv :: Environment -> m ()
  persistedValue :: Qualified Ident -> m (Maybe (SourceType, NameKind, NameVisibility))
  persistedType :: Qualified (ProperName 'TypeName) -> m (Maybe (SourceType, TypeKind))
  persistedDataConstructor :: Qualified (ProperName 'ConstructorName) -> m (Maybe (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
  peristedTypeClass :: Qualified (ProperName 'ClassName) -> m (Maybe TypeClassData)

newtype WithoutPersistenceT m a = WithoutPersistenceT (m a)
  deriving newtype (Generic, Functor, Applicative, Monad, MonadError e, MonadWriter w, MonadState s)

runWithoutPersistence :: WithoutPersistenceT m a -> m a
runWithoutPersistence (WithoutPersistenceT m) = m

instance (Monad m) => PersistEnvM (WithoutPersistenceT m) where
  initDb = pure ()
  persistEnv _ = pure ()
  persistedValue _ = pure Nothing
  persistedType _ = pure Nothing
  persistedDataConstructor _ = pure Nothing
  peristedTypeClass _ = pure Nothing

newtype SqlitePersistenceT m a = SqlitePersistenceT (ReaderT Connection m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError e, MonadWriter w, MonadState s, MonadReader Connection)

getPersistentConnection :: (Monad m) => SqlitePersistenceT m Connection
getPersistentConnection = ask

runPersistence :: Connection -> SqlitePersistenceT m a -> m a
runPersistence conn (SqlitePersistenceT m) = runReaderT m conn

instance (MonadIO m) => PersistEnvM (SqlitePersistenceT m) where
  initDb = do
    conn <- ask
    liftIO $ initEnvTables conn
  persistEnv env = do
    conn <- ask
    liftIO $ do
      -- todo: make parallel?
      E.names env & Map.toList & mapConcurrently_ (uncurry $ persistValueImpl conn)
      E.types env & Map.toList & mapConcurrently_ (uncurry $ persistTypeImpl conn)
      E.dataConstructors env & Map.toList & mapConcurrently_ (uncurry $ persistDataConstructor conn)
      E.typeSynonyms env & Map.toList & mapConcurrently_ (uncurry $ persistTypeSynonymImpl conn)
      E.typeClasses env & Map.toList & mapConcurrently_ (uncurry $ persistTypeClassImpl conn)

  persistedValue = liftWithConn persistedValueImpl
  persistedType = liftWithConn persistedTypeImpl
  persistedDataConstructor = liftWithConn persistedDataConstructorImpl
  peristedTypeClass = liftWithConn persistedTypeClassImpl

liftWithConn :: (MonadIO m) => (Connection -> a -> IO b) -> a -> SqlitePersistenceT m b
liftWithConn io a = do
  conn <- getPersistentConnection
  liftIO $ io conn a

persistValueImpl :: Connection -> Qualified Ident -> (SourceType, NameKind, NameVisibility) -> IO ()
persistValueImpl conn ident (ty, nk, nv) =
  execute
    conn
    "INSERT INTO env_values (ident, source_type, name_kind, name_visibility) VALUES (?, ?, ?, ?, ?)"
    (ident, ty, nk, nv)

persistedValueImpl :: Connection -> Qualified Ident -> IO (Maybe (SourceType, NameKind, NameVisibility))
persistedValueImpl conn ident =
  query
    conn
    "SELECT source_type, name_kind, name_visibility FROM env_values WHERE ident = ?"
    (Only ident)
    <&> head

persistTypeImpl :: Connection -> Qualified (ProperName 'TypeName) -> (SourceType, TypeKind) -> IO ()
persistTypeImpl conn ident (ty, tk) =
  execute
    conn
    "INSERT INTO env_types (type_name, source_type, type_kind) VALUES (?, ?, ?)"
    (ident, ty, tk)

persistedTypeImpl :: Connection -> Qualified (ProperName 'TypeName) -> IO (Maybe (SourceType, TypeKind))
persistedTypeImpl conn ident =
  query
    conn
    "SELECT source_type, type_kind FROM env_types WHERE type_name = ?"
    (Only ident)
    <&> head

persistDataConstructor :: Connection -> Qualified (ProperName 'ConstructorName) -> (DataDeclType, ProperName 'TypeName, SourceType, [Ident]) -> IO ()
persistDataConstructor conn ident (ddt, ty, st, idents) =
  execute
    conn
    "INSERT INTO env_data_constructors (constructor_name, data_decl_type, type_name, source_type, idents) VALUES (?, ?, ?, ?, ?)"
    (ident, ddt, ty, st, serialise idents)

persistedDataConstructorImpl :: Connection -> Qualified (ProperName 'ConstructorName) -> IO (Maybe (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
persistedDataConstructorImpl conn ident =
  query
    conn
    "SELECT data_decl_type, type_name, source_type, idents FROM env_data_constructors WHERE constructor_name = ?"
    (Only ident)
    <&> (head >>> fmap deserialiseIdents)
    where 
      deserialiseIdents (ddt, ty, st, idents) = (ddt, ty, st, deserialise idents)

persistTypeSynonymImpl :: Connection -> Qualified (ProperName 'TypeName) -> ([(Text, Maybe SourceType)], SourceType) -> IO ()
persistTypeSynonymImpl conn ident (idents, st) =
  execute
    conn
    "INSERT INTO env_type_synonyms (type_name, idents, source_type) VALUES (?, ?, ?)"
    (ident, serialise idents, st)

persistedTypeSynonymImpl :: Connection -> Qualified (ProperName 'TypeName) -> IO (Maybe ([(Text, Maybe SourceType)], SourceType))
persistedTypeSynonymImpl conn ident =
  query
    conn
    "SELECT idents, source_type FROM env_type_synonyms WHERE type_name = ?"
    (Only ident)
    <&> (head >>> fmap deserialiseIdents)
    where 
      deserialiseIdents (idents, st) = (deserialise idents, st)

persistTypeClassImpl :: Connection -> Qualified (ProperName 'ClassName) -> TypeClassData -> IO ()
persistTypeClassImpl conn ident tcd =
  execute
    conn
    "INSERT INTO env_type_classes (class_name, class) VALUES (?, ?)"
    (ident, tcd)

persistedTypeClassImpl :: Connection -> Qualified (ProperName 'ClassName) -> IO (Maybe TypeClassData)
persistedTypeClassImpl conn ident =
  query
    conn
    "SELECT class FROM env_type_classes WHERE class_name = ?"
    (Only ident)
    <&> (fmap fromOnly . head)


initEnvTables :: Connection -> IO ()
initEnvTables conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS env_values (ident TEXT PRIMARY KEY, source_type BLOB, name_kind TEXT, name_visibility TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS env_types (type_name TEXT PRIMARY KEY, source_type BLOB, type_kind TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS env_data_constructors (constructor_name TEXT PRIMARY KEY, data_decl_type TEXT, type_name TEXT, source_type BLOB, idents BLOB)"
  execute_ conn "CREATE TABLE IF NOT EXISTS env_type_synonyms (type_name TEXT PRIMARY KEY, idents BLOB, source_type BLOB)"
  execute_ conn "CREATE TABLE IF NOT EXISTS env_type_classes (class_name TEXT PRIMARY KEY, class BLOB)"