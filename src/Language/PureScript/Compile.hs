module Language.PureScript.Compile where

import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as M
import Database.SQLite.Simple (Connection)
import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Make (buildMakeActions, inferForeignModules, runMake)
import Language.PureScript.Make.Index (addAllIndexing, addDbConnection)
import System.Directory (createDirectoryIfMissing)
import Prelude

compile :: P.Options -> [(FilePath, P.Text)] -> Connection -> FilePath -> Bool -> IO (Either P.MultipleErrors [P.ExternsFile], P.MultipleErrors)
compile opts moduleFiles conn outputDir usePrefx = do
  runMake opts $ do
    ms <- CST.parseModulesFromFiles id moduleFiles
    let filePathMap = M.fromList $ map (\(fp, pm) -> (P.getModuleName $ CST.resPartial pm, Right fp)) ms
    foreigns <- inferForeignModules filePathMap
    liftIO $ createDirectoryIfMissing True outputDir
    let makeActions =
          addDbConnection conn $
            addAllIndexing conn $
              buildMakeActions outputDir filePathMap foreigns usePrefx
    P.make makeActions (map snd ms)