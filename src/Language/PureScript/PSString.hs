{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Language.PureScript.PSString
  ( PSString
  , toUTF16CodeUnits
  , decodeString
  , decodeStringEither
  , decodeStringWithReplacement
  , prettyPrintString
  , prettyPrintStringJS
  , mkString
  ) where

import Prelude
import GHC.Generics (Generic)
import Codec.Serialise (Serialise)
import Codec.Serialise qualified as Serialise
import Codec.Serialise.Decoding (decodeListLenOf, decodeWord)
import Codec.Serialise.Encoding (Encoding, encodeBreak, encodeListLen, encodeListLenIndef, encodeWord, encodeWord16)
import Control.DeepSeq (NFData)
import Control.Monad (foldM_, unless, zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Array.Byte (ByteArray(..))
import Control.Exception (try, evaluate)
import Control.Applicative ((<|>))
import Data.Char qualified as Char
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.ByteString.Short.Internal (ShortByteString(..), unsafeIndex)
import Data.List (foldl')
import GHC.Exts (Int(..), MutableByteArray#, newByteArray#, unsafeFreezeByteArray#, writeWord8Array#, (*#), (+#))
import GHC.ST (ST(..))
import GHC.Word (Word8(..))
import Data.Either (fromRight)
import Data.List (unfoldr)
import Data.Scientific (toBoundedInteger)
import Data.String (IsString(..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Unsafe (Iter(..), iter, lengthWord8)
import Data.Text.Encoding (decodeUtf16BE)
import Data.Text.Encoding.Error (UnicodeException)
import Data.Vector qualified as V
import Data.Word (Word16, Word8)
import Numeric (showHex)
import System.IO.Unsafe (unsafePerformIO)
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A

-- |
-- Strings in PureScript are sequences of UTF-16 code units, which do not
-- necessarily represent UTF-16 encoded text. For example, it is permissible
-- for a string to contain *lone surrogates,* i.e. characters in the range
-- U+D800 to U+DFFF which do not appear as a part of a surrogate pair.
--
-- The Show instance for PSString produces a string literal which would
-- represent the same data were it inserted into a PureScript source file.
--
-- Because JSON parsers vary wildly in terms of how they deal with lone
-- surrogates in JSON strings, the ToJSON instance for PSString produces JSON
-- strings where that would be safe (i.e. when there are no lone surrogates),
-- and arrays of UTF-16 code units (integers) otherwise.
--
newtype PSString = PSString ShortByteString
  deriving (Eq, Ord, Semigroup, Monoid, Generic, NFData)

instance Serialise PSString where
  encode s = encodeListLen 2 <> encodeWord 0 <> encodeUnits s
  decode = do
    decodeListLenOf 2
    tag <- decodeWord
    unless (tag == 0) $ fail "unexpected tag for PSString"
    fromCodeUnits <$> Serialise.decode

encodeUnits :: PSString -> Encoding
encodeUnits s
  | n == 0 = encodeListLen 0
  | otherwise = encodeListLenIndef <> go 0
  where
  n = unitCount s
  go i
    | i >= n = encodeBreak
    | otherwise = encodeWord16 (unitAt s i) <> go (i + 1)

unitCount :: PSString -> Int
unitCount (PSString s) = SBS.length s `div` 2

unitAt :: PSString -> Int -> Word16
unitAt (PSString s) i = fromIntegral (unsafeIndex s (2 * i)) `shiftL` 8 .|. fromIntegral (unsafeIndex s (2 * i + 1))

toUTF16CodeUnits :: PSString -> [Word16]
toUTF16CodeUnits s = map (unitAt s) [0 .. unitCount s - 1]

data Units s = Units (MutableByteArray# s)

createUnits :: Int -> (forall s. Units s -> ST s ()) -> PSString
createUnits (I# n) fill = runST $ ST $ \s0 ->
  case newByteArray# (n *# 2#) s0 of
    (# s1, m #) -> case fill (Units m) of
      ST f -> case f s1 of
        (# s2, () #) -> case unsafeFreezeByteArray# m s2 of
          (# s3, a #) -> (# s3, PSString (ShortByteString (ByteArray a)) #)

writeUnit :: Units s -> Int -> Word16 -> ST s ()
writeUnit (Units m) (I# i) w = ST $ \s ->
  case writeWord8Array# m (i *# 2#) hi s of
    s' -> (# writeWord8Array# m (i *# 2# +# 1#) lo s', () #)
  where
  !(W8# hi) = fromIntegral (w `shiftR` 8)
  !(W8# lo) = fromIntegral w

charUnits :: Char -> Int
charUnits c = if fromEnum c > 0xFFFF then 2 else 1

writeChar :: Units s -> Int -> Char -> ST s Int
writeChar u i c
  | n > 0xFFFF = do
      writeUnit u i (toWord (h + 0xD800))
      writeUnit u (i + 1) (toWord (l + 0xDC00))
      pure (i + 2)
  | otherwise = do
      writeUnit u i (toWord n)
      pure (i + 1)
  where
  n = fromEnum c
  (h, l) = divMod (n - 0x10000) 0x400

fromCodeUnits :: [Word16] -> PSString
fromCodeUnits us = createUnits (length us) (\u -> zipWithM_ (writeUnit u) [0 ..] us)

instance Show PSString where
  show = show . codePoints

-- |
-- Decode a PSString to a String, representing any lone surrogates as the
-- reserved code point with that index. Warning: if there are any lone
-- surrogates, converting the result to Text via Data.Text.pack will result in
-- loss of information as those lone surrogates will be replaced with U+FFFD
-- REPLACEMENT CHARACTER. Because this function requires care to use correctly,
-- we do not export it.
--
codePoints :: PSString -> String
codePoints = map (either (Char.chr . fromIntegral) id) . decodeStringEither

-- |
-- Decode a PSString as UTF-16 text. Lone surrogates will be replaced with
-- U+FFFD REPLACEMENT CHARACTER
--
decodeStringWithReplacement :: PSString -> String
decodeStringWithReplacement = map (fromRight '\xFFFD') . decodeStringEither

-- |
-- Decode a PSString as UTF-16. Lone surrogates in the input are represented in
-- the output with the Left constructor; characters which were successfully
-- decoded are represented with the Right constructor.
--
decodeStringEither :: PSString -> [Either Word16 Char]
decodeStringEither = unfoldr decode . toUTF16CodeUnits
  where
  decode :: [Word16] -> Maybe (Either Word16 Char, [Word16])
  decode (h:l:rest) | isLead h && isTrail l = Just (Right (unsurrogate h l), rest)
  decode (c:rest) | isSurrogate c = Just (Left c, rest)
  decode (c:rest) = Just (Right (toChar c), rest)
  decode [] = Nothing

  unsurrogate :: Word16 -> Word16 -> Char
  unsurrogate h l = toEnum ((toInt h - 0xD800) * 0x400 + (toInt l - 0xDC00) + 0x10000)

-- |
-- Attempt to decode a PSString as UTF-16 text. This will fail (returning
-- Nothing) if the argument contains lone surrogates.
--
decodeString :: PSString -> Maybe Text
decodeString (PSString s) = hush . decodeEither $ SBS.fromShort s
  where
  -- Based on a similar function from Data.Text.Encoding for utf8. This is a
  -- safe usage of unsafePerformIO because there are no side effects after
  -- handling any thrown UnicodeExceptions.
  decodeEither :: ByteString -> Either UnicodeException Text
  decodeEither = unsafePerformIO . try . evaluate . decodeUtf16BE

  hush = either (const Nothing) Just

instance IsString PSString where
  fromString a = createUnits (foldl' (\k c -> k + charUnits c) 0 a) (\u -> foldM_ (writeChar u) 0 a)

instance A.ToJSON PSString where
  toJSON str =
    case decodeString str of
      Just t -> A.toJSON t
      Nothing -> A.toJSON (toUTF16CodeUnits str)

instance A.FromJSON PSString where
  parseJSON a = jsonString <|> arrayOfCodeUnits
    where
    jsonString = fromString <$> A.parseJSON a

    arrayOfCodeUnits = fromCodeUnits <$> parseArrayOfCodeUnits a

    parseArrayOfCodeUnits :: A.Value -> A.Parser [Word16]
    parseArrayOfCodeUnits = A.withArray "array of UTF-16 code units" (traverse parseCodeUnit . V.toList)

    parseCodeUnit :: A.Value -> A.Parser Word16
    parseCodeUnit b = A.withScientific "two-byte non-negative integer" (maybe (A.typeMismatch "" b) return . toBoundedInteger) b

-- |
-- Pretty print a PSString, using PureScript escape sequences.
--
prettyPrintString :: PSString -> Text
prettyPrintString s = "\"" <> foldMap encodeChar (decodeStringEither s) <> "\""
  where
  encodeChar :: Either Word16 Char -> Text
  encodeChar (Left c) = "\\x" <> showHex' 6 c
  encodeChar (Right c)
    | c == '\t' = "\\t"
    | c == '\r' = "\\r"
    | c == '\n' = "\\n"
    | c == '"'  = "\\\""
    | c == '\''  = "\\\'"
    | c == '\\' = "\\\\"
    | shouldPrint c = T.singleton c
    | otherwise = "\\x" <> showHex' 6 (Char.ord c)

  -- Note we do not use Data.Char.isPrint here because that includes things
  -- like zero-width spaces and combining punctuation marks, which could be
  -- confusing to print unescaped.
  shouldPrint :: Char -> Bool
  -- The standard space character, U+20 SPACE, is the only space char we should
  -- print without escaping
  shouldPrint ' ' = True
  shouldPrint c =
    Char.generalCategory c `elem`
      [ Char.UppercaseLetter
      , Char.LowercaseLetter
      , Char.TitlecaseLetter
      , Char.OtherLetter
      , Char.DecimalNumber
      , Char.LetterNumber
      , Char.OtherNumber
      , Char.ConnectorPunctuation
      , Char.DashPunctuation
      , Char.OpenPunctuation
      , Char.ClosePunctuation
      , Char.InitialQuote
      , Char.FinalQuote
      , Char.OtherPunctuation
      , Char.MathSymbol
      , Char.CurrencySymbol
      , Char.ModifierSymbol
      , Char.OtherSymbol
      ]

-- |
-- Pretty print a PSString, using JavaScript escape sequences. Intended for
-- use in compiled JS output.
--
prettyPrintStringJS :: PSString -> Text
prettyPrintStringJS s = "\"" <> foldMap encodeChar (toUTF16CodeUnits s) <> "\""
  where
  encodeChar :: Word16 -> Text
  encodeChar c | c > 0xFF = "\\u" <> showHex' 4 c
  encodeChar c | c > 0x7E || c < 0x20 = "\\x" <> showHex' 2 c
  encodeChar c | toChar c == '\b' = "\\b"
  encodeChar c | toChar c == '\t' = "\\t"
  encodeChar c | toChar c == '\n' = "\\n"
  encodeChar c | toChar c == '\v' = "\\v"
  encodeChar c | toChar c == '\f' = "\\f"
  encodeChar c | toChar c == '\r' = "\\r"
  encodeChar c | toChar c == '"'  = "\\\""
  encodeChar c | toChar c == '\\' = "\\\\"
  encodeChar c = T.singleton $ toChar c

showHex' :: Enum a => Int -> a -> Text
showHex' width c =
  let hs = showHex (fromEnum c) "" in
  T.pack (replicate (width - length hs) '0' <> hs)

isLead :: Word16 -> Bool
isLead h = h >= 0xD800 && h <= 0xDBFF

isTrail :: Word16 -> Bool
isTrail l = l >= 0xDC00 && l <= 0xDFFF

isSurrogate :: Word16 -> Bool
isSurrogate c = isLead c || isTrail c

toChar :: Word16 -> Char
toChar = toEnum . fromIntegral

toWord :: Int -> Word16
toWord = fromIntegral

toInt :: Word16 -> Int
toInt = fromIntegral

mkString :: Text -> PSString
mkString t = createUnits (T.foldl' (\k c -> k + charUnits c) 0 t) (\u -> go u 0 0)
  where
  end = lengthWord8 t
  go :: Units s -> Int -> Int -> ST s ()
  go u i j
    | j >= end = pure ()
    | otherwise = do
        let Iter c d = iter t j
        i' <- writeChar u i c
        go u i' (j + d)
