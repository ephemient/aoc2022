{-|
Module:         Day9
Description:    <https://adventofcode.com/2022/day/9 Day 9: Rope Bridge>
-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Day9 (day9a, day9b) where

import Control.Arrow (first, second)
import Data.List (scanl')
import qualified Data.Set as Set (fromList, size)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack, words)
import qualified Data.Text.Read as T (decimal)

follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow (hx, hy) (tx, ty)
  | abs dx <= 1 && abs dy <= 1 = (tx, ty)
  | abs dx > abs dy = (hx - signum dx, hy)
  | abs dx < abs dy = (hx, hy - signum dy)
  | otherwise = (hx - signum dx, hy - signum dy)
  where
    dx = hx - tx
    dy = hy - ty

move :: Char -> (Int, Int) -> (Int, Int)
move 'L' = first pred
move 'R' = first succ
move 'U' = second pred
move 'D' = second succ

expand :: Text -> [Char]
expand (T.words -> [T.unpack -> [d], T.decimal -> Right (n, "")]) = replicate n d

day9 :: Text -> [[(Int, Int)]]
day9 = iterate (scanl' (flip follow) (0, 0)) . scanl' (flip move) (0, 0) . concatMap expand . T.lines

day9a :: Text -> Int
day9a = Set.size . Set.fromList . (!! 1) . day9

day9b :: Text -> Int
day9b = Set.size . Set.fromList . (!! 9) . day9
