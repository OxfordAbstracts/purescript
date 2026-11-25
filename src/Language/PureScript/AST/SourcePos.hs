-- |
-- Source position information
--
module Language.PureScript.AST.SourcePos
  ( SourceAnn
  , SourcePos(SourcePos, sourcePosLine, sourcePosColumn)
  , displaySourcePos
  , displaySourcePosShort
  , SourceSpan(..)
  , displayStartEndPos
  , displayStartEndPosShort
  , displaySourceSpan
  , internalModuleSourceSpan
  , nullSourceSpan
  , nullSourceAnn
  , pattern NullSourceSpan
  , pattern NullSourceAnn
  , nonEmptySpan
  , widenSourceSpan
  , widenSourceAnn
  ) where

import Prelude

import Codec.CBOR.Decoding (decodeInt, decodeListLenOf, decodeString, decodeWordOf)
import Codec.CBOR.Encoding (encodeInt, encodeListLen, encodeString, encodeWord)
import Codec.Serialise (Serialise(..))
import Control.DeepSeq (NFData(..))
import Data.Aeson ((.=), (.:))
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.PureScript (shareDecodedText)
import Language.PureScript.Comments (Comment)
import Data.Aeson qualified as A
import Data.Text qualified as T
import System.FilePath (makeRelative)

-- | Source annotation - position information and comments.
type SourceAnn = (SourceSpan, [Comment])

-- | Source position information
data SourcePos = PackedSourcePos {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32
  deriving (Eq, Ord)

pattern SourcePos :: Int -> Int -> SourcePos
pattern SourcePos{sourcePosLine, sourcePosColumn} <-
  PackedSourcePos (fromIntegral -> sourcePosLine) (fromIntegral -> sourcePosColumn)
  where
  SourcePos line column = PackedSourcePos (fromIntegral line) (fromIntegral column)

{-# COMPLETE SourcePos #-}

instance Show SourcePos where
  showsPrec d (SourcePos line column) = showParen (d >= 11) $
    showString "SourcePos {sourcePosLine = " . showsPrec 0 line .
    showString ", sourcePosColumn = " . showsPrec 0 column .
    showChar '}'

instance NFData SourcePos where
  rnf (PackedSourcePos _ _) = ()

instance Serialise SourcePos where
  encode (SourcePos line column) =
    encodeListLen 3 <> encodeWord 0 <> encodeInt line <> encodeInt column
  decode = do
    decodeListLenOf 3
    decodeWordOf 0
    SourcePos <$> decodeInt <*> decodeInt

displaySourcePos :: SourcePos -> Text
displaySourcePos sp =
  "line " <> T.pack (show (sourcePosLine sp)) <>
    ", column " <> T.pack (show (sourcePosColumn sp))

displaySourcePosShort :: SourcePos -> Text
displaySourcePosShort sp =
  T.pack (show (sourcePosLine sp)) <>
    ":" <> T.pack (show (sourcePosColumn sp))

instance A.ToJSON SourcePos where
  toJSON SourcePos{..} =
    A.toJSON [sourcePosLine, sourcePosColumn]

instance A.FromJSON SourcePos where
  parseJSON arr = do
    [line, col] <- A.parseJSON arr
    return $ SourcePos line col

data SourceSpan = SourceSpan
  { spanName :: !Text
    -- ^ Source name
  , spanStart :: {-# UNPACK #-} !SourcePos
    -- ^ Start of the span
  , spanEnd :: {-# UNPACK #-} !SourcePos
    -- ^ End of the span
  } deriving (Show, Eq, Ord)

instance NFData SourceSpan where
  rnf (SourceSpan _ _ _) = ()

instance Serialise SourceSpan where
  encode (SourceSpan name start end) =
    encodeListLen 4 <> encodeWord 0 <> encodeString name <> encode start <> encode end
  decode = do
    decodeListLenOf 4
    decodeWordOf 0
    name <- decodeString >>= shareDecodedText
    SourceSpan name <$> decode <*> decode

displayStartEndPos :: SourceSpan -> Text
displayStartEndPos sp =
  "(" <>
  displaySourcePos (spanStart sp) <> " - " <>
  displaySourcePos (spanEnd sp) <> ")"

displayStartEndPosShort :: SourceSpan -> Text
displayStartEndPosShort sp =
  displaySourcePosShort (spanStart sp) <> " - " <>
  displaySourcePosShort (spanEnd sp)

displaySourceSpan :: FilePath -> SourceSpan -> Text
displaySourceSpan relPath sp =
  T.pack (makeRelative relPath (T.unpack (spanName sp))) <> ":" <>
    displayStartEndPosShort sp <> " " <>
    displayStartEndPos sp

instance A.ToJSON SourceSpan where
  toJSON SourceSpan{..} =
    A.object [ "name"  .= spanName
             , "start" .= spanStart
             , "end"   .= spanEnd
             ]

instance A.FromJSON SourceSpan where
  parseJSON = A.withObject "SourceSpan" $ \o ->
    SourceSpan     <$>
      o .: "name"  <*>
      o .: "start" <*>
      o .: "end"

internalModuleSourceSpan :: Text -> SourceSpan
internalModuleSourceSpan name = SourceSpan name (SourcePos 0 0) (SourcePos 0 0)

nullSourceSpan :: SourceSpan
nullSourceSpan = internalModuleSourceSpan ""

nullSourceAnn :: SourceAnn
nullSourceAnn = (nullSourceSpan, [])

pattern NullSourceSpan :: SourceSpan
pattern NullSourceSpan = SourceSpan "" (SourcePos 0 0) (SourcePos 0 0)

pattern NullSourceAnn :: SourceAnn
pattern NullSourceAnn = (NullSourceSpan, [])

nonEmptySpan :: SourceAnn -> Maybe SourceSpan
nonEmptySpan (NullSourceSpan, _) = Nothing
nonEmptySpan (ss, _) = Just ss

widenSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
widenSourceSpan NullSourceSpan b = b
widenSourceSpan a NullSourceSpan = a
widenSourceSpan (SourceSpan n1 s1 e1) (SourceSpan n2 s2 e2) =
  SourceSpan n (min s1 s2) (max e1 e2)
  where
  n | n1 == ""  = n2
    | otherwise = n1

widenSourceAnn :: SourceAnn -> SourceAnn -> SourceAnn
widenSourceAnn (s1, _) (s2, _) = (widenSourceSpan s1 s2, [])
