module Language.PureScript.Compile where

import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as M
import Database.SQLite.Simple (Connection)
import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Make (buildMakeActions, inferForeignModules, runMake)
import Language.PureScript.Make.Index (addAllIndexing)
import System.Directory (createDirectoryIfMissing)
import System.IO.UTF8 (readUTF8FilesT)
import Prelude

compile :: P.Options -> [FilePath] -> Connection -> FilePath -> Bool -> IO (Either P.MultipleErrors [P.ExternsFile], P.MultipleErrors)
compile opts input conn outputDir usePrefx = do
  moduleFiles <- readUTF8FilesT input
  runMake opts $ do
    ms <- CST.parseModulesFromFiles id moduleFiles
    let filePathMap = M.fromList $ map (\(fp, pm) -> (P.getModuleName $ CST.resPartial pm, Right fp)) ms
    foreigns <- inferForeignModules filePathMap
    liftIO $ createDirectoryIfMissing True outputDir
    let makeActions =
          addAllIndexing conn $
            buildMakeActions outputDir filePathMap foreigns usePrefx
    P.make makeActions (map snd ms)