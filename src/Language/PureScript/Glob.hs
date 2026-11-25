module Language.PureScript.Glob where

import Prelude

import Control.Concurrent.Async.Chunked (forConcurrentlyChunked)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.List (nub, (\\))
import Data.Text qualified as T
import System.FilePath.Glob (glob)
import System.IO (hPutStrLn, stderr)
import System.IO.UTF8 (readUTF8FileT)

data PSCGlobs = PSCGlobs
  { pscInputGlobs :: [FilePath]
  , pscInputGlobsFromFile :: Maybe FilePath
  , pscExcludeGlobs :: [FilePath]
  , pscWarnFileTypeNotFound :: FilePath -> IO ()
  }

toInputGlobs :: PSCGlobs -> IO [FilePath]
toInputGlobs (PSCGlobs {..}) = do
  globsFromFile <- inputGlobsFromFile pscInputGlobsFromFile
  included <- globWarningOnMisses pscWarnFileTypeNotFound $ nub $ pscInputGlobs <> globsFromFile
  excluded <- globWarningOnMisses pscWarnFileTypeNotFound pscExcludeGlobs
  pure $ included \\ excluded

inputGlobsFromFile :: Maybe FilePath -> IO [FilePath]
inputGlobsFromFile globsFromFile = do
  mbInputsFromFile <- traverse readUTF8FileT globsFromFile
  let
    excludeBlankLines = not . T.null . T.strip
    excludeComments = not . T.isPrefixOf "#"
    toInputs = map (T.unpack . T.strip) . filter (\x -> excludeBlankLines x && excludeComments x) . T.lines
  pure $ foldMap toInputs mbInputsFromFile

globWarningOnMisses :: (String -> IO ()) -> [FilePath] -> IO [FilePath]
globWarningOnMisses warn patterns = do
  matches <- forConcurrentlyChunked 8 patterns glob
  for_ (zip patterns matches) $ \(pattern', paths) ->
    when (null paths) $ warn pattern'
  pure (concat matches)

warnFileTypeNotFound :: String -> String -> IO ()
warnFileTypeNotFound pursCmd = hPutStrLn stderr . ("purs " <> pursCmd <> ": No files found using pattern: " ++)
