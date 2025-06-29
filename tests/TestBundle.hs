{-# LANGUAGE DoAndIfThenElse #-}

module TestBundle where

import Prelude ()
import Prelude.Compat

import qualified Language.PureScript as P
import Language.PureScript.Bundle
import Language.PureScript.Interactive.IO (readNodeProcessWithExitCode)

import Data.Function (on)
import Data.List (minimumBy)

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except

import System.Exit
import System.FilePath
import System.IO
import System.IO.UTF8
import qualified System.FilePath.Glob as Glob

import TestUtils
import Test.Hspec

spec :: SpecWith SupportModules
spec =
  context "Bundle examples" $
    beforeAllWith ((<$> createOutputFile logfile) . (,)) $ do
      bundleTestCases <- runIO $ getTestFiles "bundle"
      forM_ bundleTestCases $ \testPurs -> do
        it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile, bundle and run without error") $ \(support, outputFile) ->
          assertBundles support testPurs outputFile
  where

  -- Takes the test entry point from a group of purs files - this is determined
  -- by the file with the shortest path name, as everything but the main file
  -- will be under a subdirectory.
  getTestMain :: [FilePath] -> FilePath
  getTestMain = minimumBy (compare `on` length)

assertBundles
  :: SupportModules
  -> [FilePath]
  -> Handle
  -> Expectation
assertBundles support inputFiles outputFile = do
  (result, _) <- compile support inputFiles
  case result of
    Left errs -> expectationFailure . P.prettyPrintMultipleErrors P.defaultPPEOptions $ errs
    Right _ -> do
      jsFiles <- concat <$> Glob.globDir [Glob.compile "*/*.js", Glob.compile "*/foreign.cjs"] modulesDir
      let entryPoint = modulesDir </> "index.cjs"
      let entryModule = [(`ModuleIdentifier` Regular) "Main"]
      bundled <- runExceptT $ do
        input <- forM jsFiles $ \filename -> do
          js <- liftIO $ readUTF8File filename
          mid <- guessModuleIdentifier filename
          length js `seq` return (mid, Just filename, js)
        bundleSM input entryModule (Just "Main") "PS" (Just entryPoint) Nothing
      case bundled of
          Right (_, js) -> do
            writeUTF8File entryPoint js
            nodeResult <- readNodeProcessWithExitCode Nothing [entryPoint] ""
            hPutStrLn outputFile $ "\n" <> takeFileName (last inputFiles) <> ":"
            case nodeResult of
              Right (ExitSuccess, out, err)
               | not (null err) -> expectationFailure $ "Test wrote to stderr:\n\n" <> err
               | not (null out) && trim (last (lines out)) == "Done" -> hPutStr outputFile out
               | otherwise -> expectationFailure $ "Test did not finish with 'Done':\n\n" <> out
              Right (ExitFailure _, _, err) -> expectationFailure err
              Left err -> expectationFailure err
          Left err -> expectationFailure $ "Could not bundle: " ++ show err

logfile :: FilePath
logfile = "bundle-tests.out"
