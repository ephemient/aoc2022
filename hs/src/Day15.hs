{-|
Module:         Day15
Description:    <https://adventofcode.com/2022/day/15 Day 15: Beacon Exclusion Zone>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day15 (day15a, day15b) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap (empty, foldlWithKey', maxViewWithKey, minViewWithKey, singleton, splitLookup, unions)
import qualified Data.IntSet as IntSet (fromList, size)
import Data.Ix (rangeSize)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T (lines, stripPrefix)
import qualified Data.Text.Read as T (decimal, signed)
import GHC.Exts (the)

parseLine :: (Integral a) => Text -> ((a, a), (a, a))
parseLine line
  | Just line <- T.stripPrefix "Sensor at x=" line
  , Right (x0, line) <- T.signed T.decimal line
  , Just line <- T.stripPrefix ", y=" line
  , Right (y0, line) <- T.signed T.decimal line
  , Just line <- T.stripPrefix ": closest beacon is at x=" line
  , Right (x1, line) <- T.signed T.decimal line
  , Just line <- T.stripPrefix ", y=" line
  , Right (y1, "") <- T.signed T.decimal line
  = ((x0, y0), (x1, y1))

intervalAdd :: IntMap Int -> (Int, Int) -> IntMap Int
intervalAdd intervals (lo, hi) = IntMap.unions [los', IntMap.singleton lo' hi'', his'] where
    (los, mid, his) = IntMap.splitLookup lo intervals
    (los', lo', hi') = mergeDown los lo $ maybe hi (max hi) mid
    (his', hi'') = mergeUp his hi'
    mergeDown m x y
      | Just ((z, t), m') <- IntMap.maxViewWithKey m, x <= t + 1
      = mergeDown m' (min x z) (max y t)
      | otherwise = (m, x, y)
    mergeUp m x
      | Just ((y, z), m') <- IntMap.minViewWithKey m, x + 1 >= y
      = mergeUp m' $ max x z
      | otherwise = (m, x)

intervalSize :: IntMap Int -> Int
intervalSize = IntMap.foldlWithKey' (\s lo hi -> s + rangeSize (lo, hi)) 0

intervalGaps :: Int -> Int -> IntMap Int -> [Int]
intervalGaps lo hi m
  | lo > hi = []
  | Just ((y, z), m') <- IntMap.minViewWithKey m
  = if z < lo then intervalGaps lo hi m' else [lo..y - 1] ++ intervalGaps (z + 1) hi m'
  | otherwise = [lo..hi]

day15a :: Int -> Text -> Int
day15a y input = intervalSize intervals - beacons where
    inputs = parseLine <$> T.lines input
    intervals = foldl' intervalAdd IntMap.empty
      [ (x0 - dx, x0 + dx)
      | ((x0, y0), (x1, y1)) <- inputs
      , let dx = abs (x0 - x1) + abs (y0 - y1) - abs (y - y0)
      , dx >= 0
      ]
    beacons = IntSet.size . IntSet.fromList . map fst . filter ((== y) . snd) $ snd <$> inputs

day15b :: Int -> Text -> Int
day15b size input = the
  [ 4000000 * x + y
  | y <- [0..size]
  , let intervals = foldl' intervalAdd IntMap.empty
          [ (lo, hi)
          | ((x0, y0), (x1, y1)) <- parseLine <$> T.lines input
          , let dx = abs (x0 - x1) + abs (y0 - y1) - abs (y - y0)
          , let lo = max 0 $ x0 - dx
          , let hi = min size $ x0 + dx
          , lo <= hi
          ]
  , x <- intervalGaps 0 size intervals
  ]
