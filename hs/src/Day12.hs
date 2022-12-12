{-|
Module:         Day12
Description:    <https://adventofcode.com/2022/day/12 Day 12: Hill Climbing Algorithm>
-}
module Day12 (day12a, day12b) where

import Control.Arrow (first)
import Data.Char (ord)
import Data.Ix (inRange)
import Data.List (foldl')
import qualified Data.Sequence as Seq (Seq((:<|), Empty), (><), fromList, singleton)
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
    go _ Seq.Empty = []

day12a :: Text -> Maybe Int
day12a input = lookup 'E' $ first get <$> bfs next start where
    lines = T.lines input
    start:_ = [(y, x) | (y, line) <- zip [0..] lines, (x, 'S') <- zip [0..] $ T.unpack line]
    heights = V.fromList lines
    get (y, x) = heights V.! y `T.index` x
    next (y, x) = filter (ok $ get (y, x)) [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
    ok a (y, x) | Just line <- heights V.!? y, inRange (0, T.length line - 1) x =
        let b = T.index line x
         in if b == 'E' then a == 'z' else ord b - ord (if a == 'S' then 'a' else a) <= 1
    ok _ _ = False

day12b :: Text -> Maybe Int
day12b input = lookup 'a' $ first get <$> bfs next start where
    lines = T.lines input
    start:_ = [(y, x) | (y, line) <- zip [0..] lines, (x, 'E') <- zip [0..] $ T.unpack line]
    heights = V.fromList lines
    get (y, x) = heights V.! y `T.index` x
    next (y, x) = filter (ok $ get (y, x)) [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]
    ok a (y, x) | Just line <- heights V.!? y, inRange (0, T.length line - 1) x =
        let b = T.index line x
         in if a == 'E' then b == 'z' else ord a - ord b <= 1
    ok _ _ = False
