module Codec.CBOR.PureScript
  ( cborArray
  , cborArrayHeader
  , cborItem
  , cborWord
  ) where

import Prelude

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as BU
import Data.List (foldl')
import Data.Word (Word64)

skipCborItem :: B.ByteString -> Int -> Int
skipCborItem bytes = item
  where
  len = B.length bytes
  byteAt i = BU.unsafeIndex bytes i
  argument = cborArgument bytes
  argumentEnd = cborArgumentEnd bytes

  item i
    | i < 0 || i >= len = invalid
    | otherwise = case header `shiftR` 5 of
        0 -> argumentEnd i
        1 -> argumentEnd i
        2 -> chunked i
        3 -> chunked i
        4 | info == 31 -> untilBreak item (i + 1)
          | otherwise -> items 1 (argument i) (argumentEnd i)
        5 | info == 31 -> untilBreak (item . item) (i + 1)
          | otherwise -> items 2 (argument i) (argumentEnd i)
        6 -> item (argumentEnd i)
        _ | info < 24 -> i + 1
          | info <= 27 -> argumentEnd i
          | otherwise -> invalid
    where
    header = byteAt i
    info = header .&. 31

  chunked i
    | byteAt i .&. 31 == 31 = untilBreak chunk (i + 1)
    | j < 0 || n > fromIntegral (len - j) = invalid
    | otherwise = j + fromIntegral n
    where
    j = argumentEnd i
    n = argument i
    chunk k
      | k >= 0 && k < len && byteAt k `shiftR` 5 == byteAt i `shiftR` 5 && byteAt k .&. 31 /= 31 = chunked k
      | otherwise = invalid

  items :: Word64 -> Word64 -> Int -> Int
  items perEntry n j
    | j < 0 || n > fromIntegral (len - j) = invalid
    | otherwise = go (n * perEntry) j
    where
    go 0 k = k
    go m k = go (m - 1) (item k)

  untilBreak step k
    | k < 0 || k >= len = invalid
    | byteAt k == 0xff = k + 1
    | otherwise = untilBreak step (step k)

cborArrayHeader :: B.ByteString -> Int -> Maybe (Maybe Word64, Int)
cborArrayHeader bytes i
  | i < 0 || i >= B.length bytes || header `shiftR` 5 /= 4 = Nothing
  | header .&. 31 == 31 = Just (Nothing, i + 1)
  | end < 0 = Nothing
  | otherwise = Just (Just (cborArgument bytes i), end)
  where
  header = BU.unsafeIndex bytes i
  end = cborArgumentEnd bytes i

cborArray :: B.ByteString -> (Int -> Maybe (a, Int)) -> Int -> Maybe ([a], Int)
cborArray bytes element i = cborArrayHeader bytes i >>= \case
  (Nothing, start) -> untilBreak [] start
  (Just count, start) -> counted [] (count :: Word64) start
  where
  len = B.length bytes

  untilBreak acc k
    | k < 0 || k >= len = Nothing
    | BU.unsafeIndex bytes k == 0xff = Just (reverse acc, k + 1)
    | otherwise = element k >>= \(a, next) -> untilBreak (a : acc) next

  counted acc n k
    | k < 0 = Nothing
    | n == 0 = Just (reverse acc, k)
    | otherwise = element k >>= \(a, next) -> counted (a : acc) (n - 1) next

cborItem :: B.ByteString -> Int -> Maybe ((Int, Int), Int)
cborItem bytes i
  | end < 0 = Nothing
  | otherwise = Just ((i, end), end)
  where
  end = skipCborItem bytes i

cborWord :: B.ByteString -> Int -> Maybe Word64
cborWord bytes i
  | i < 0 || i >= B.length bytes || BU.unsafeIndex bytes i `shiftR` 5 /= 0 || cborArgumentEnd bytes i < 0 = Nothing
  | otherwise = Just (cborArgument bytes i)

invalid :: Int
invalid = -1

cborArgumentEnd :: B.ByteString -> Int -> Int
cborArgumentEnd bytes i = if end <= B.length bytes then end else invalid
  where
  end = case BU.unsafeIndex bytes i .&. 31 of
    24 -> i + 2
    25 -> i + 3
    26 -> i + 5
    27 -> i + 9
    info | info < 24 -> i + 1
         | otherwise -> maxBound

cborArgument :: B.ByteString -> Int -> Word64
cborArgument bytes i = case BU.unsafeIndex bytes i .&. 31 of
  24 -> readBE 1
  25 -> readBE 2
  26 -> readBE 4
  27 -> readBE 8
  info -> fromIntegral info
  where
  readBE :: Int -> Word64
  readBE n = foldl' (\acc o -> acc `shiftL` 8 .|. fromIntegral (BU.unsafeIndex bytes (i + 1 + o))) 0 [0 .. n - 1]
