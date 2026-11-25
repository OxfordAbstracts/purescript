-- |
-- Common pretty-printing utility functions
--
module Language.PureScript.Pretty.Common where

import Prelude

import Data.List (elemIndices, intersperse)
import Data.Text (Text)
import Data.Text qualified as T

import Language.PureScript.AST (SourcePos(..), SourceSpan(..), nullSourceSpan)
import Language.PureScript.CST.Lexer (isUnquotedKey)

import Text.PrettyPrint.Boxes (Box(..), emptyBox, text, top, vcat, (//))
import Text.PrettyPrint.Boxes qualified as Box

parensT :: Text -> Text
parensT s = "(" <> s <> ")"

parensPos :: (Emit gen) => gen -> gen
parensPos s = emit "(" <> s <> emit ")"

-- |
-- Generalize intercalate slightly for monoids
--
intercalate :: Monoid m => m -> [m] -> m
intercalate x xs = mconcat (intersperse x xs)

class (Monoid gen) => Emit gen where
  emit :: Text -> gen
  addMapping :: SourceSpan -> gen

data SMap = SMap Text SourcePos SourcePos

-- |
-- String with length and source-map entries
--
newtype StrPos = StrPos (SourcePos, Text, [SMap])

-- |
-- Make a monoid where append consists of concatenating the string part, adding the lengths
-- appropriately and advancing source mappings on the right hand side to account for
-- the length of the left.
--
instance Semigroup StrPos where
  StrPos (a,b,c) <> StrPos (a',b',c') = StrPos (a `addPos` a', b <> b', c ++ (bumpPos a <$> c'))

instance Monoid StrPos where
  mempty = StrPos (SourcePos 0 0, "", [])

  mconcat ms =
    let s' = foldMap (\(StrPos(_, s, _)) -> s) ms
        (p, maps) = foldl plus (SourcePos 0 0, []) ms
    in
        StrPos (p, s', concat $ reverse maps)
    where
      plus :: (SourcePos, [[SMap]]) -> StrPos -> (SourcePos, [[SMap]])
      plus (a, c) (StrPos (a', _, c')) = (a `addPos` a', (bumpPos a <$> c') : c)

instance Emit StrPos where
  -- Augment a string with its length (rows/column)
  emit str =
    -- TODO(Christoph): get rid of T.unpack
    let newlines = elemIndices '\n' (T.unpack str)
        index = if null newlines then 0 else last newlines + 1
    in
    StrPos (SourcePos { sourcePosLine = length newlines, sourcePosColumn = T.length str - index }, str, [])

  -- Add a new mapping entry for given source position with initially zero generated position
  addMapping ss@SourceSpan { spanName = file, spanStart = startPos } = StrPos (zeroPos, mempty, [ mapping | ss /= nullSourceSpan ])
    where
      mapping = SMap file startPos zeroPos
      zeroPos = SourcePos 0 0

newtype PlainString = PlainString ([Text] -> [Text])

instance Semigroup PlainString where
  PlainString f <> PlainString g = PlainString (f . g)

instance Monoid PlainString where
  mempty = PlainString id

runPlainString :: PlainString -> Text
runPlainString (PlainString chunks) = T.concat (chunks [])

instance Emit PlainString where
  emit s = PlainString (s :)
  addMapping _ = mempty

addMapping' :: (Emit gen) => Maybe SourceSpan -> gen
addMapping' (Just ss) = addMapping ss
addMapping' Nothing = mempty

bumpPos :: SourcePos -> SMap -> SMap
bumpPos p (SMap f s g) = SMap f s $ p `addPos` g

addPos :: SourcePos -> SourcePos -> SourcePos
addPos (SourcePos n m) (SourcePos 0 m') = SourcePos n (m + m')
addPos (SourcePos n _) (SourcePos n' m') = SourcePos (n + n') m'


-- |
-- Number of characters per indentation level
--
blockIndent :: Int
blockIndent = 4

objectKeyRequiresQuoting :: Text -> Bool
objectKeyRequiresQuoting = not . isUnquotedKey

-- | Place a box before another, vertically when the first box takes up multiple lines.
before :: Box -> Box -> Box
before b1 b2 | rows b1 > 1 = b1 // b2
             | otherwise = b1 Box.<> b2

beforeWithSpace :: Box -> Box -> Box
beforeWithSpace b1 = before (b1 Box.<> text " ")

-- | Place a Box on the bottom right of another
endWith :: Box -> Box -> Box
endWith l r = l Box.<> vcat top [emptyBox (rows l - 1) (cols r), r]
