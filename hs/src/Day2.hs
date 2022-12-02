{-|
Module:         Day2
Description:    <https://adventofcode.com/2022/day/2 Day 2: Rock Paper Scissors>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day2 (day2a, day2b) where

import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T (head, lines, words)

parse :: Text -> (Int, Int)
parse line = (parse1 lhs, parse1 rhs) where
    [lhs, rhs] = T.words line
    parse1 = succ . (`mod` 3) . (`mod` 23) . pred . ord . T.head

score :: Int -> Int -> Int
score lhs rhs = (1 + rhs - lhs) `mod` 3 * 3 + rhs

day2a :: Text -> Int
day2a input = sum $ uncurry score . parse <$> T.lines input

day2b :: Text -> Int
day2b input = sum [score lhs $ 1 + (lhs + rhs) `mod` 3 | (lhs, rhs) <- parse <$> T.lines input]
