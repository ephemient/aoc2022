{-|
Module:         Day12
Description:    <https://adventofcode.com/2022/day/12 Day 12: Hill Climbing Algorithm>
-}
{-# LANGUAGE ViewPatterns #-}
module Day12 (day12) where

import Control.Arrow (first, second)
import Data.Char (ord)
import Data.Ix (inRange)
import Data.List (foldl')
import qualified Data.Sequence as Seq (Seq((:<|)), (><), fromList, singleton)
import qualified Data.Set as Set (insert, notMember, singleton)
import Data.Text (Text)
import qualified Data.Text as T (index, length, lines, unpack)
import qualified Data.Vector as V ((!), (!?), fromList)

bfs :: (Ord a) => (a -> [a]) -> a -> [(a, Int)]
bfs next start = go (Set.singleton start) (Seq.singleton (start, 0)) where
    go seen ((a, !n) Seq.:<| q) = (a, n) : go seen' q' where
        seen' = foldl' (flip Set.insert) seen next'
        q' = q Seq.>< Seq.fromList [(a', n + 1) | a' <- next']
        next' = filter (`Set.notMember` seen) $ next a
    go _ _ = []

ord' :: Char -> Int
ord' 'S' = ord 'a'
ord' 'E' = ord 'z'
ord' a = ord a

day12 :: Text -> (Maybe Int, Maybe Int)
day12 input = foldr ans (Nothing, Nothing) $ bfs next start where
    lines = T.lines input
    start:_ = [(y, x) | (y, line) <- zip [0..] lines, (x, 'E') <- zip [0..] $ T.unpack line]
    heights = V.fromList lines
    get (y, x) = heights V.! y `T.index` x
    next (y, x) = filter (ok $ get (y, x)) [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
    ok a (y, x) | Just line <- heights V.!? y =
        inRange (0, T.length line - 1) x && ord' a - ord' (T.index line x) <= 1
    ok _ _ = False
    ans (get -> 'S', n) = first . const $ Just n
    ans (get -> 'a', n) = second . const $ Just n
    ans _ = id
