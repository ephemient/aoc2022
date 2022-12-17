{-|
Module:         Day17
Description:    <https://adventofcode.com/2022/day/17 Day 17: Pyroclastic Flow>
-}
module Day17 (day17) where

import Control.Arrow (second)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet (findMax, fromDistinctAscList, fromList, intersection, insert, mapMonotonic, member, notMember, singleton, union)
import Data.List (foldl', scanl')
import qualified Data.Map as Map (empty, insert, lookup)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (index, length, lines)

rocks :: [[String]]
rocks = [["####"], [".#.", "###", ".#."], ["###", "..#", "..#"], ["#", "#", "#", "#"], ["##", "##"]]

height :: IntSet -> Int
height = (`div` 7) . IntSet.findMax

fall :: Text -> (Int, IntSet) -> [String] -> (Int, IntSet)
fall jet (i, set) rock = fall' i 2 (maxY + 4) where
    maxX = 7 - maximum (length <$> rock)
    maxY = height set
    offset x y = [7 * ry + rx | (ry, row) <- zip [y..] rock, (rx, '#') <- zip [x..] row]
    fall' i x y
      | any (flip IntSet.member set) $ offset x'' (y - 1)
      = (i', cleanup . IntSet.union set . IntSet.fromList $ offset x'' y)
      | otherwise = fall' i' x'' (y - 1)
      where
        c = T.index jet i
        i' = (i + 1) `mod` T.length jet
        x' | '<' <- c = x - 1 | '>' <- c = x + 1
        x'' = if x' < 0 || x' > maxX || any (flip IntSet.member set) (offset x' y) then x else x'
        stop = any (flip IntSet.member set) $ offset x'' (y - 1)
    cleanup set = reachable (IntSet.singleton p0) [p0] where
        p0 = 7 * (height set + 1)
        reachable seen [] = IntSet.intersection set seen
        reachable seen (p:q)
          | IntSet.member p set = reachable (IntSet.insert p seen) q
          | otherwise = reachable (foldl' (flip IntSet.insert) seen ps) $ ps ++ q where
            ps = filter (flip IntSet.notMember seen) $ p - 7 :
                [p - 1 | p `mod` 7 /= 0] ++ [p + 1 | p `mod` 7 /= 6] ++ [p + 7 | p + 6 < p0]

findCycle :: (Ord a) => [a] -> Maybe (Int, Int)
findCycle xs = listToMaybe [(i, j) | (j, Just i) <- zip [0..] $ zipWith Map.lookup xs ixs] where
    ixs = scanl' (flip $ uncurry Map.insert) Map.empty $ zip xs [0..]

day17 :: Int -> Text -> Int
day17 n input
  | Just (i, j) <- findCycle . zip (cycle [1..length rocks]) $ second normalize <$> take n states
  = let height1 = height (snd $ states !! i)
        height2 = height (snd $ states !! j)
        (q, r) = (n - i) `divMod` (j - i)
     in height (snd $ states !! (i + r)) + q * (height2 - height1)
  | otherwise = height . snd $ states !! n
  where
    [jet] = T.lines input
    states = scanl' (fall jet) (0, IntSet.fromDistinctAscList [0..6]) (cycle rocks)
    normalize set = IntSet.mapMonotonic (subtract $ 7 * height set) set
