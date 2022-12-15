{-|
Module:         Day15
Description:    <https://adventofcode.com/2022/day/15 Day 15: Beacon Exclusion Zone>
-}
{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
module Day15 (day15a, day15b) where

import Control.Monad ((>=>), foldM)
import Control.Monad.Writer (execWriter, tell)
import qualified Data.IntSet as IntSet (fromList, size)
import Data.Ix (rangeSize)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set (empty, lookupMax, lookupMin, singleton, spanAntitone, unions)
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

intervalAdd :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
intervalAdd intervals (lo, hi) = Set.unions [los, mid', his] where
    (los, rest) = Set.spanAntitone (\(_, y) -> y < lo - 1) intervals
    (mid, his) = Set.spanAntitone (\(x, _) -> x <= hi + 1) rest
    mid' = Set.singleton $ if
      | Just (lo', _) <- Set.lookupMin mid
      , Just (_, hi') <- Set.lookupMax mid
      -> (min lo lo', max hi hi')
      | otherwise -> (lo, hi)

intervalSize :: Set (Int, Int) -> Int
intervalSize = foldl' (flip $ (+) . rangeSize) 0

intervalGaps :: Int -> Int -> Set (Int, Int) -> [Int]
intervalGaps lo hi = execWriter . (foldM f lo >=> g) where
    f x (y, z) = z + 1 <$ tell [x..y - 1]
    g x = tell [x..hi]

day15a :: Int -> Text -> Int
day15a y input = intervalSize intervals - IntSet.size beacons where
    inputs = parseLine <$> T.lines input
    intervals = foldl' intervalAdd Set.empty
      [ (x0 - dx, x0 + dx)
      | ((x0, y0), (x1, y1)) <- inputs
      , let dx = abs (x0 - x1) + abs (y0 - y1) - abs (y - y0)
      , dx >= 0
      ]
    beacons = IntSet.fromList . map fst . filter ((== y) . snd) $ snd <$> inputs

day15b :: Int -> Text -> Int
day15b size input = the
  [ 4000000 * x + y
  | y <- [0..size]
  , let intervals = foldl' intervalAdd Set.empty
          [ (lo, hi)
          | ((x0, y0), (x1, y1)) <- parseLine <$> T.lines input
          , let dx = abs (x0 - x1) + abs (y0 - y1) - abs (y - y0)
          , let lo = max 0 $ x0 - dx
          , let hi = min size $ x0 + dx
          , lo <= hi
          ]
  , x <- intervalGaps 0 size intervals
  ]
