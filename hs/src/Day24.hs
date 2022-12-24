{-|
Module:         Day24
Description:    <https://adventofcode.com/2022/day/24 Day 24: Blizzard Basin>
-}
{-# LANGUAGE NondecreasingIndentation, ViewPatterns #-}
module Day24 (day24a, day24b) where

import Control.Arrow ((&&&), second)
import Data.Either (partitionEithers)
import qualified Data.Heap as Heap (FstMinPolicy, insert, singleton, view)
import Data.List (foldl', transpose)
import qualified Data.Map as Map ((!), (!?), fromList, fromListWith)
import Data.Maybe (maybeToList)
import Data.Semigroup (Arg(Arg), Max(Max), Min(Min))
import Data.Set (Set)
import qualified Data.Set as Set (difference, empty, fromDistinctAscList, fromList, insert, lookupMax, lookupMin, member, notMember, toList)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import Data.Tuple (swap)

parse :: (Enum a, Num a, Ord a) => Text -> Maybe ((a, a), (a, a), [Set (a, a)])
parse input = (,, spaces) <$> Set.lookupMin startingSpaces <*> Set.lookupMax startingSpaces where
    deltas = Map.fromList [('^', decY), ('>', incX), ('v', incY), ('<', decX)]
    (Set.fromDistinctAscList -> startingSpaces, blizzards) = partitionEithers $ do
        (y, line) <- zip [0..] $ T.lines input
        (x, char) <- zip [0..] $ T.unpack line
        if char == '.' then pure $ Left (y, x) else Right <$> do
        delta <- maybeToList $ deltas Map.!? char
        pure ((y, x), delta)
    potentialSpaces = startingSpaces <> Set.fromList (fst <$> blizzards)
    xy = Map.fromListWith (<>) $ second (Min &&& Max) <$> Set.toList potentialSpaces
    yx = Map.fromListWith (<>) $ second (Min &&& Max) . swap <$> Set.toList potentialSpaces
    incX (y, x) | x >= maxX = (y, minX) | otherwise = (y, x + 1)
      where (Min minX, Max maxX) = xy Map.! y
    decX (y, x) | x <= minX = (y, maxX) | otherwise = (y, x - 1)
      where (Min minX, Max maxX) = xy Map.! y
    incY (y, x) | y >= maxY = (minY, x) | otherwise = (y + 1, x)
      where (Min minY, Max maxY) = yx Map.! x
    decY (y, x) | y <= minY = (maxY, x) | otherwise = (y - 1, x)
      where (Min minY, Max maxY) = yx Map.! x
    spaces = (potentialSpaces `Set.difference`) . Set.fromList <$> transpose
        [iterate delta $ delta pos | (pos, delta) <- blizzards]

search :: (Ord a, Ord b) => (a -> [(b, a)]) -> (b, a) -> [a]
search next = search' Set.empty . Heap.singleton @Heap.FstMinPolicy where
    search' seen (Heap.view -> Just ((b, a), heap))
      | Set.member a seen = search' seen heap
      | otherwise = a : search' seen' heap' where
            seen' = Set.insert a seen
            heap' = foldl' (flip Heap.insert) heap .
                filter (flip Set.notMember seen' . snd) $ next a
    search' _ _ = []

search' :: (Int, Int) -> (Int, Int) -> [Set (Int, Int)] -> Maybe (Int, [Set (Int, Int)])
search' start end@(endY, endX) spaces = do
    Arg n spaces' <- lookup end $ search next (0, (start, Arg 0 spaces))
    pure (n, spaces')
  where
    next ((y, x), Arg n (space:spaces')) =
      [ (abs (y' - endY) + abs (x' - endX) + n, (pos, Arg (n + 1) spaces'))
      | pos@(y', x') <- [(y - 1, x), (y, x - 1), (y, x), (y, x + 1), (y + 1, x)]
      , pos `Set.member` space
      ]

day24a :: Text -> Maybe Int
day24a input = do
    (start, end, spaces) <- parse input
    fst <$> search' start end spaces

day24b :: Text -> Maybe Int
day24b input = do
    (start, end, spaces) <- parse input
    (n1, spaces') <- search' start end spaces
    (n2, spaces'') <- search' end start spaces'
    (n3, _) <- search' start end spaces''
    pure $ n1 + n2 + n3
