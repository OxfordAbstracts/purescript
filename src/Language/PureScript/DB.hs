module Language.PureScript.DB where 

import Protolude
import Database.SQLite.Simple (Connection, open)
import System.FilePath ((</>))

mkConnection :: FilePath -> IO Connection
mkConnection outputDir =  
  open (outputDir </> dbFile)

dbFile :: FilePath
dbFile = "purescript.sqlite"