{-|
Module:         Day10
Description:    <https://adventofcode.com/2022/day/10 Day 10: Cathode-Ray Tube>
-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Day10 (day10a, day10b) where

import qualified Data.IntMap as IntMap (fromDistinctAscList, lookupLT)
import Data.Ix (inRange)
import Data.List (intercalate, unfoldr)
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T (lines, stripPrefix)
import qualified Data.Text.Read as T (decimal, signed)

parse :: (Integral a) => Text -> [(Int, a)]
parse = scanl f (0, 1) . T.lines where
    f (i, x) "noop" = (i + 1, x)
    f (i, x) (T.stripPrefix "addx " -> Just (T.signed T.decimal -> Right (dx, ""))) = (i + 2, x + dx)

day10a :: Text -> Int
day10a input = sum . zipWith (*) taps . map snd $ mapMaybe (flip IntMap.lookupLT signals) taps where
    signals = IntMap.fromDistinctAscList $ parse input
    taps = [20, 60, 100, 140, 180, 220]

day10b :: Text -> String
day10b input = intercalate "\n" $ take 6 rows where
    sprites = concat $ unfoldr f (0, 1, parse input)
    f (i, x, (j, y):signals) = Just (replicate (j - i) x, (j, y, signals))
    f (_, x, _) = Just (repeat x, undefined)
    rows = zipWith draw [0..] <$> chunksOf 40 sprites
    draw i x = if inRange (-1, 1) $ x - i then '\x2593' else '\x2591'
