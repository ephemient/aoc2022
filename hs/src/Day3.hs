{-|
Module:         Day3
Description:    <https://adventofcode.com/2022/day/3 Day 3: Rucksack Reorganization>
-}
module Day3 (day3a, day3b) where

import Data.Char (ord)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet (fromList, intersection, toList)
import Data.List (foldl1')
import Data.List.Split (chunksOf)
import Data.Text (Text)
import qualified Data.Text as T (length, lines, splitAt, unpack)

items :: Text -> IntSet
items = IntSet.fromList . map prio . T.unpack where
    prio c = if c >= 'a' then ord c - ord 'a' + 1 else ord c - ord 'A' + 27

day3a :: Text -> Int
day3a input = sum $ do
    line <- T.lines input
    let (a, b) = T.splitAt (T.length line `div` 2) line
    IntSet.toList $ items a `IntSet.intersection` items b

day3b :: Text -> Int
day3b input = sum $ do
    group <- chunksOf 3 $ T.lines input
    IntSet.toList . foldl1' IntSet.intersection $ items <$> group
