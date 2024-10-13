module Language.PureScript.DB where 

import Protolude
import Database.SQLite.Simple (Connection, open)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

mkConnection :: FilePath -> IO Connection
mkConnection outputDir =  do
  createDirectoryIfMissing True outputDir
  open (outputDir </> dbFile)

dbFile :: FilePath
dbFile = "purescript.sqlite"