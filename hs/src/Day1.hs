{-|
Module:         Day1
Description:    <https://adventofcode.com/2022/day/1 Day 1: Calorie Counting>
-}
module Day1 (day1a, day1b) where

import Common (readEntire)
import Data.Either (isLeft, rights)
import Data.List (maximum, sortOn)
import Data.List.Split (splitWhen)
import Data.Ord (Down(Down))
import Data.Text (Text)
import qualified Data.Text as T (lines)
import qualified Data.Text.Read as T (decimal)

parse :: Text -> [Int]
parse = map (sum . rights) . splitWhen isLeft . map (readEntire T.decimal) . T.lines

day1a :: Text -> Int
day1a = maximum . parse

day1b :: Text -> Int
day1b = sum . take 3 . sortOn Down . parse
