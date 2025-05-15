module Language.PureScript.DB where

import Database.SQLite.Simple (Connection, open)
import Protolude
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath ((</>))

mkConnection :: FilePath -> IO (FilePath, Connection)
mkConnection outputDir = do
  createDirectoryIfMissing True outputDir
  path <- mkDbPath outputDir
  putErrLn $ "Opening sqlite database at " <> path
  conn <- open path
  pure (path, conn)

mkDbPath :: FilePath -> IO FilePath
mkDbPath outputDir = canonicalizePath $ outputDir </> dbFile

dbFile :: FilePath
dbFile = "purescript.sqlite"