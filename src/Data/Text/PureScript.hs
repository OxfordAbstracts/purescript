-- |
-- This module contains internal extensions to Data.Text.
--
module Data.Text.PureScript (spanUpTo, shareDecodedText) where

import Prelude

import Codec.CBOR.Decoding (Decoder, liftST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOArray, newArray)
import Data.Bits (xor, (.&.))
import Data.Text.Array qualified as A
import Data.Text.Internal (Text(..), empty, text)
import Data.Text.Unsafe (Iter(..), iter)
import System.IO.Unsafe (unsafePerformIO)

shareDecodedText :: Text -> Decoder s Text
shareDecodedText = liftST . unsafeIOToST . shareText

recentTextSlots :: Int
recentTextSlots = 65536

recentTexts :: IOArray Int Text
recentTexts = unsafePerformIO (newArray (0, recentTextSlots - 1) empty)
{-# NOINLINE recentTexts #-}

shareText :: Text -> IO Text
shareText decoded = do
  let slot = hashBytes decoded .&. (recentTextSlots - 1)
  recent <- unsafeRead recentTexts slot
  if recent == decoded
    then pure recent
    else decoded <$ unsafeWrite recentTexts slot decoded

hashBytes :: Text -> Int
hashBytes (Text arr off len) = go off 14695981039346656037
  where
  end = off + len
  go :: Int -> Word -> Int
  go i h
    | i == end = fromIntegral (h `xor` (h `div` 4294967296))
    | otherwise = go (i + 1) ((h `xor` fromIntegral (A.unsafeIndex arr i)) * 1099511628211)

-- | /O(n)/ 'spanUpTo', applied to a number @n@, predicate @p@, and text @t@,
-- returns a pair whose first element is the longest prefix (possibly empty) of
-- @t@ of length less than or equal to @n@ of elements that satisfy @p@, and
-- whose second is the remainder of the text.
{-# INLINE spanUpTo #-}
spanUpTo :: Int -> (Char -> Bool) -> Text -> (Text, Text)
spanUpTo n p t@(Text arr off len) = (hd, tl)
  where hd = text arr off k
        tl = text arr (off + k) (len - k)
        !k = loop n 0
        loop !n' !i | n' > 0 && i < len && p c = loop (n' - 1) (i + d)
                    | otherwise                = i
            where Iter c d = iter t i
