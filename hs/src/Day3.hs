{-|
Module:         Day3
Description:    <https://adventofcode.com/2022/day/3 Day 3: Rucksack Reorganization>
-}
module Day3 (day3a, day3b) where

import Data.Bits (Bits((.&.), setBit, testBit, zeroBits), FiniteBits(countTrailingZeros, finiteBitSize))
import Data.Char (ord)
import Data.List (foldl1')
import Data.List.Split (chunksOf)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.Text as T (foldl', length, lines, splitAt)

items :: Text -> Word64
items = T.foldl' (\acc c -> setBit acc $ prio c) zeroBits where
    prio c = if c >= 'a' then ord c - ord 'a' + 1 else ord c - ord 'A' + 27

bits :: (FiniteBits b) => b -> [Int]
bits x = filter (testBit x) [0 .. finiteBitSize x - 1]

day3a :: Text -> Int
day3a input = sum $ do
    line <- T.lines input
    let (a, b) = T.splitAt (T.length line `div` 2) line
    bits $ items a .&. items b

day3b :: Text -> Int
day3b input = sum $ do
    group <- chunksOf 3 $ T.lines input
    pure . countTrailingZeros . foldl1' (.&.) $ items <$> group
