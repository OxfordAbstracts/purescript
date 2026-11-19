module Main where

import Codec.Serialise (Serialise(encode), readFileDeserialise, hPutSerialise)
import qualified Codec.CBOR.Write as W
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.ByteString.Builder (hPutBuilder)
import Data.List (sort)
import qualified FastWrite
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getAllocationCounter)
import Language.PureScript.Externs (ExternsFile)
import System.Directory (doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (Handle, IOMode(WriteMode), withBinaryFile)

data Writer = Lazy | Handle | Fast deriving (Eq, Show)

{-# NOINLINE encodeOne #-}
encodeOne :: Writer -> Handle -> ExternsFile -> IO ()
encodeOne mode h ef = case mode of
  Lazy -> hPutSerialise h ef
  Handle -> hPutBuilder h (W.toBuilder (encode ef))
  Fast -> hPutBuilder h (FastWrite.toBuilder (encode ef))

main :: IO ()
main = do
  [root] <- getArgs
  modules <- sort <$> listDirectory root
  withBinaryFile "/dev/null" WriteMode $ \h ->
    forM_ (zip [0::Int ..] modules) $ \(i, m) -> do
      let path = root </> m </> "externs.cbor"
      exists <- doesFileExist path
      when exists $ do
        ef <- readFileDeserialise path >>= evaluate . force
        let modes = take 3 . drop (i `mod` 3) . cycle $ [Lazy,Handle,Fast]
        forM_ modes $ \mode -> do
          before <- getAllocationCounter
          start <- getMonotonicTimeNSec
          encodeOne mode h ef
          stop <- getMonotonicTimeNSec
          after <- getAllocationCounter
          putStrLn $ unwords [m, show mode, show (before - after), show (stop - start)]
