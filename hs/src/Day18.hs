{-|
Module:         Day18
Description:    <https://adventofcode.com/2022/day/18 Day 18: Boiling Boulders>
-}
module Day18 (day18a, day18b) where

import Control.Arrow ((&&&))
import Data.Ix (inRange)
import Data.List (foldl')
import Data.Semigroup (Max(Max), Min(Min))
import qualified Data.Set as Set (fromList, insert, member, notMember, singleton)
import Data.Text (Text)
import qualified Data.Text as T (cons, lines, unpack, snoc)

neighbors :: (Num a) => (a, a, a) -> [(a, a, a)]
neighbors (x, y, z) =
    [(x - 1, y, z), (x + 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1)]

day18a :: Text -> Int
day18a input = length . filter (`Set.notMember` points') $ points >>= neighbors where
    points = read @(Int, Int, Int) . T.unpack . T.cons '(' . flip T.snoc ')' <$> T.lines input
    points' = Set.fromList points

day18b :: Text -> Int
day18b input = length . filter (`Set.member` outside) $ points >>= neighbors where
    points = read @(Int, Int, Int) . T.unpack . T.cons '(' . flip T.snoc ')' <$> T.lines input
    points' = Set.fromList points
    ((Min minX, Max maxX), (Min minY, Max maxY), (Min minZ, Max maxZ)) = mconcat
        [(Min &&& Max $ x, Min &&& Max $ y, Min &&& Max $ z) | (x, y, z) <- points]
    outside = let start = (minX - 1, minY - 1, minZ - 1) in dfs (Set.singleton start) [start]
    dfs seen [] = seen
    dfs seen (point:q) = dfs (foldl' (flip Set.insert) seen next) $ next ++ q where
        next = filter ok $ neighbors point
        ok point@(x, y, z) =
            inRange (minX - 1, maxX + 1) x &&
            inRange (minY - 1, maxY + 1) y &&
            inRange (minZ - 1, maxZ + 1) z &&
            Set.notMember point points' &&
            Set.notMember point seen
