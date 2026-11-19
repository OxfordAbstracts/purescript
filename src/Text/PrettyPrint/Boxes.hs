-----------------------------------------------------------------------------
--
-- Module      :  Text.PrettyPrint.Boxes
-- Copyright   :  (c) Brent Yorgey 2008
-- License     :  BSD3 (boxes 0.1.5, see LICENSE)
--
-----------------------------------------------------------------------------

module Text.PrettyPrint.Boxes
  ( Box(rows, cols)
  , nullBox
  , emptyBox
  , char
  , text
  , para
  , (<>)
  , (<+>)
  , hcat
  , hsep
  , (//)
  , (/+/)
  , vcat
  , vsep
  , punctuateH
  , punctuateV
  , Alignment
  , left
  , right
  , top
  , bottom
  , center1
  , center2
  , moveLeft
  , moveRight
  , moveUp
  , moveDown
  , alignHoriz
  , alignVert
  , align
  , render
  , printBox
  ) where

import Prelude hiding ((<>))

import Control.Monad (foldM_, when)
import Control.Monad.ST (ST)
import Data.Array.ST (STUArray, newArray, runSTUArray, writeArray)
import Data.Array.Unboxed (elems)
import Data.Foldable (foldl', for_, toList)
import Data.List (intersperse)
import Data.String (IsString(..))

data Box = Box
  { rows :: Int
  , cols :: Int
  , content :: Content
  }
  deriving (Show)

instance IsString Box where
  fromString = text

data Alignment
  = AlignFirst
  | AlignCenter1
  | AlignCenter2
  | AlignLast
  deriving (Eq, Read, Show)

data Content
  = Blank
  | Text String
  | Row [Box]
  | Col [Box]
  | SubBox Alignment Alignment Box
  deriving (Show)

top :: Alignment
top = AlignFirst

bottom :: Alignment
bottom = AlignLast

left :: Alignment
left = AlignFirst

right :: Alignment
right = AlignLast

center1 :: Alignment
center1 = AlignCenter1

center2 :: Alignment
center2 = AlignCenter2

nullBox :: Box
nullBox = emptyBox 0 0

emptyBox :: Int -> Int -> Box
emptyBox r c = Box r c Blank

char :: Char -> Box
char c = Box 1 1 (Text [c])

text :: String -> Box
text t = Box 1 (length t) (Text t)

(<>) :: Box -> Box -> Box
l <> r = hcat top [l, r]

(<+>) :: Box -> Box -> Box
l <+> r = hcat top [l, emptyBox 0 1, r]

(//) :: Box -> Box -> Box
t // b = vcat left [t, b]

(/+/) :: Box -> Box -> Box
t /+/ b = vcat left [t, emptyBox 1 0, b]

hcat :: Foldable f => Alignment -> f Box -> Box
hcat a bs = Box h w (Row $ map (alignVert a h) bsl)
  where
  (w, h) = sumMax cols 0 rows bsl
  bsl = toList bs

hsep :: Foldable f => Int -> Alignment -> f Box -> Box
hsep sep a = punctuateH a (emptyBox 0 sep)

vcat :: Foldable f => Alignment -> f Box -> Box
vcat a bs = Box h w (Col $ map (alignHoriz a w) bsl)
  where
  (h, w) = sumMax rows 0 cols bsl
  bsl = toList bs

sumMax :: (Num n, Ord b, Foldable f) => (a -> n) -> b -> (a -> b) -> f a -> (n, b)
sumMax f defaultMax g as = foldr go (,) as 0 defaultMax
  where
  go a r n b = (r $! f a + n) $! g a `max` b

vsep :: Foldable f => Int -> Alignment -> f Box -> Box
vsep sep a = punctuateV a (emptyBox sep 0)

punctuateH :: Foldable f => Alignment -> Box -> f Box -> Box
punctuateH a p bs = hcat a (intersperse p (toList bs))

punctuateV :: Foldable f => Alignment -> Box -> f Box -> Box
punctuateV a p bs = vcat a (intersperse p (toList bs))

para :: Alignment -> Int -> String -> Box
para a width t = alignVert top (length ls) . vcat a . map text $ ls
  where
  ls = flow width t

flow :: Int -> String -> [String]
flow width = map (take width) . finish . foldl' addWord ([], (0, [])) . words
  where
  addWord :: ([(Int, [String])], (Int, [String])) -> String -> ([(Int, [String])], (Int, [String]))
  addWord (full, line@(len, ws)) w
    | len == 0 || len + length w + 1 <= width = (full, (len + length w + 1, w : ws))
    | otherwise = (line : full, (length w, [w]))

  finish :: ([(Int, [String])], (Int, [String])) -> [String]
  finish (full, line@(len, _)) =
    map (unwords . reverse . snd) . reverse $ if len == 0 then full else line : full

alignHoriz :: Alignment -> Int -> Box -> Box
alignHoriz a c b = align a AlignFirst (rows b) c b

alignVert :: Alignment -> Int -> Box -> Box
alignVert a r b = align AlignFirst a r (cols b) b

align :: Alignment -> Alignment -> Int -> Int -> Box -> Box
align ah av r c = Box r c . SubBox ah av

moveUp :: Int -> Box -> Box
moveUp n b = alignVert top (rows b + n) b

moveDown :: Int -> Box -> Box
moveDown n b = alignVert bottom (rows b + n) b

moveLeft :: Int -> Box -> Box
moveLeft n b = alignHoriz left (cols b + n) b

moveRight :: Int -> Box -> Box
moveRight n b = alignHoriz right (cols b + n) b

render :: Box -> String
render box = elems canvas
  where
  height = max 0 (rows box)
  width = max 0 (cols box)
  stride = width + 1
  canvas = runSTUArray (do
    cells <- newArray (0, height * stride - 1) ' '
    for_ [1 .. height] $ \row -> writeArray cells (row * stride - 1) '\n'
    paint cells stride (Region 0 height 0 width) 0 0 (rows box) (cols box) (content box)
    pure cells)

data Region = Region Int Int Int Int

paint :: STUArray s Int Char -> Int -> Region -> Int -> Int -> Int -> Int -> Content -> ST s ()
paint cells stride (Region rowFrom rowTo colFrom colTo) y x r c boxContent =
  when (visibleRowFrom < visibleRowTo && visibleColFrom < visibleColTo) $ case boxContent of
    Blank ->
      pure ()
    Text t ->
      when (visibleRowFrom == y) $
        for_ (takeWhile ((< visibleColTo) . fst) . dropWhile ((< visibleColFrom) . fst) $ zip [x ..] t) $ \(col, ch) ->
          writeArray cells (y * stride + col) ch
    Row bs ->
      foldM_ (\childX (Box _ childCols childContent) ->
        (childX + max 0 childCols) <$ paintChild y childX r childCols childContent) x bs
    Col bs ->
      foldM_ (\childY (Box childRows _ childContent) ->
        (childY + max 0 childRows) <$ paintChild childY x childRows c childContent) y bs
    SubBox ah av (Box childRows childCols childContent) ->
      paintChild (y + alignedOffset av r childRows) (x + alignedOffset ah c childCols) childRows childCols childContent
  where
  visibleRowFrom = max rowFrom y
  visibleRowTo = min rowTo (y + r)
  visibleColFrom = max colFrom x
  visibleColTo = min colTo (x + c)
  paintChild = paint cells stride (Region visibleRowFrom visibleRowTo visibleColFrom visibleColTo)

alignedOffset :: Alignment -> Int -> Int -> Int
alignedOffset a outer inner = leading outer - leading (max 0 inner)
  where
  leading :: Int -> Int
  leading n = case a of
    AlignFirst -> 0
    AlignLast -> n
    AlignCenter1 -> (n + 1) `div` 2
    AlignCenter2 -> n `div` 2

printBox :: Box -> IO ()
printBox = putStr . render
