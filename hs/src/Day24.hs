{-|
Module:         Day24
Description:    <https://adventofcode.com/2022/day/24 Day 24: Blizzard Basin>
-}
{-# LANGUAGE NondecreasingIndentation, ViewPatterns #-}
module Day24 (day24a, day24b) where

import qualified Data.Heap as Heap (FstMinPolicy, insert, singleton, view)
import Data.List (foldl')
import qualified Data.Set as Set (insert, notMember, singleton)
import Data.Text (Text)
import qualified Data.Text as T (index, length, lines)
import Data.Tuple (swap)
import Data.Vector (Vector)
import qualified Data.Vector as V ((!), fromList, last, length)

search :: Vector Text -> (Int, Int) -> (Int, Int) -> Int -> Maybe Int
search lines start end@(endX, endY) startTime = go (Set.singleton (start, startTime)) $
    Heap.singleton @Heap.FstMinPolicy (0, (start, startTime)) where
    isFree (x, y) time
      | y < 0 || y >= V.length lines || x < 1 || x > T.length line - 2 = False
      | y == 0 || y == V.length lines - 1 = T.index line x == '.'
      | T.index line ((x - 1 + time) `mod` (T.length line - 2) + 1) == '<' = False
      | T.index line ((x - 1 - time) `mod` (T.length line - 2) + 1) == '>' = False
      | T.index (lines V.! ((y - 1 + time) `mod` (V.length lines - 2) + 1)) x == '^' = False
      | T.index (lines V.! ((y - 1 - time) `mod` (V.length lines - 2) + 1)) x == 'v' = False
      | otherwise = True
      where line = lines V.! y
    go seen (Heap.view -> Just ((_, (pos@(x, y), time)), heap))
      | pos == end = Just time
      | otherwise = go seen' heap' where
        choices = filter (\state@(pos, time) -> isFree pos time && Set.notMember state seen) $
            (, time + 1) <$> [(x - 1, y), (x, y - 1), (x, y), (x, y + 1), (x + 1, y)]
        seen' = foldl' (flip Set.insert) seen choices
        heap' = foldl' (flip Heap.insert) heap
            [(time + abs (x - endX) + abs (y - endY), choice) | choice@((x, y), time) <- choices]
    go _ _ = Nothing

day24a :: Text -> Maybe Int
day24a input = search lines (1, 0) (T.length (V.last lines) - 2, V.length lines - 1) 0 where
    lines = V.fromList $ T.lines input

day24b :: Text -> Maybe Int
day24b input = search lines start end 0 >>= search lines end start >>= search lines start end where
    lines = V.fromList $ T.lines input
    start = (1, 0)
    end = (T.length (V.last lines) - 2, V.length lines - 1)
