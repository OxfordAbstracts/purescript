{-# LANGUAGE BangPatterns #-}
module Main where

import Codec.CBOR.Write (toBuilder)
import Codec.Serialise (Serialise(encode), readFileDeserialise, writeFileSerialise)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM_)
import Data.ByteString.Builder (hPutBuilder)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats)
import Language.PureScript.Externs (ExternsFile)
import qualified FastWrite
import qualified StockWrite
import System.Environment (getArgs)
import System.IO (IOMode(WriteMode), withBinaryFile)
import System.Mem (performGC)

main :: IO ()
main = do
  [src, prefix, mode, repeats] <- getArgs
  ef <- readFileDeserialise src >>= evaluate . force :: IO ExternsFile
  let run tag f = do
        performGC
        before <- getRTSStats
        start <- getMonotonicTimeNSec
        f (prefix ++ tag) ef
        end <- getMonotonicTimeNSec
        performGC
        after <- getRTSStats
        print (tag, fromIntegral (end - start) / 1.0e6 :: Double, allocated_bytes after - allocated_bytes before)
      methods = [("installed", writeHandle toBuilder), ("stock", writeHandle StockWrite.toBuilder), ("inline-text", writeHandle FastWrite.toBuilder)]
      selected = if mode == "both" then methods else filter ((== mode) . fst) methods
  forM_ [1 .. read repeats :: Int] $ \i ->
    forM_ (if odd i then selected else reverse selected) $ \(tag, w) -> run tag w

{-# NOINLINE writeHandle #-}
writeHandle toB path ef = withBinaryFile path WriteMode $ \h -> hPutBuilder h (toB (encode ef))
