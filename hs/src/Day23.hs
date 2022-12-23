{-|
Module:         Day23
Description:    <https://adventofcode.com/2022/day/23 Day 23: Unstable Diffusion>
-}
module Day23 (day23a, day23b) where

import Control.Arrow ((&&&), (***))
import Data.List (findIndex, scanl', tails)
import qualified Data.Map as Map (elems, filter, fromListWith, keysSet)
import Data.Maybe (fromJust)
import Data.Semigroup (Max(Max), Min(Min))
import Data.Set (Set)
import qualified Data.Set as Set (difference, fromList, member, size, toList, union)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)

data Direction = N | S | W | E deriving (Bounded, Enum)

dirs :: [[Direction]]
dirs = take 4 <$> tails (cycle [minBound..maxBound])

sides :: (Enum a, Num a) => Direction -> (a, a) -> [(a, a)]
sides N (x, y) = [(x', y - 1) | x' <- [x - 1..x + 1]]
sides S (x, y) = [(x', y + 1) | x' <- [x - 1..x + 1]]
sides W (x, y) = [(x - 1, y') | y' <- [y - 1..y + 1]]
sides E (x, y) = [(x + 1, y') | y' <- [y - 1..y + 1]]

move :: (Num a) => Direction -> (a, a) -> (a, a)
move N (x, y) = (x, y - 1)
move S (x, y) = (x, y + 1)
move W (x, y) = (x - 1, y)
move E (x, y) = (x + 1, y)

neighbors :: (Enum a, Eq a, Num a) => (a, a) -> [(a, a)]
neighbors (x, y) = [(x', y') | x' <- [x - 1..x + 1] , y' <- [y - 1..y + 1] , x /= x' || y /= y']

step :: (Enum a, Num a, Ord a) => Set (a, a) -> [Direction] -> Set (a, a)
step state dirs = state `Set.difference`
    Set.fromList (concat $ Map.elems proposals) `Set.union` Map.keysSet proposals where
    proposals = Map.filter ((== 1) . length) $ Map.fromListWith (<>)
      [ (move dir pos, [pos])
      | pos <- Set.toList state
      , any (`Set.member` state) $ neighbors pos
      , dir <- take 1 $ filter (not . any (`Set.member` state) . (`sides` pos)) dirs
      ]

parse :: Text -> Set (Int, Int)
parse input = Set.fromList
    [(x, y) | (y, line) <- zip [0..] $ T.lines input , (x, '#') <- zip [0..] $ T.unpack line]

day23a :: Text -> Int
day23a input = (maxX - minX + 1) * (maxY - minY + 1) - Set.size state where
    state = scanl' step (parse input) dirs !! 10
    ((Min minX, Max maxX), (Min minY, Max maxY)) =
        foldMap ((Min &&& Max) *** (Min &&& Max)) state

day23b :: Text -> Int
day23b input = fromJust (findIndex id . zipWith (==) states $ drop 1 states) + 1 where
    states = scanl' step (parse input) dirs
