{-|
Module:         Day22
Description:    <https://adventofcode.com/2022/day/22 Day 22: Monkey Map>
-}
{-# LANGUAGE LambdaCase, MultiWayIf, NondecreasingIndentation, OverloadedStrings, ParallelListComp, RecordWildCards, ScopedTypeVariables, TransformListComp, TypeFamilies, ViewPatterns #-}
module Day22 (day22a, day22b) where

import Control.Arrow (first)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Ix (inRange)
import Data.List (find, foldl', foldl1', groupBy)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map ((!?), fromList)
import Data.Maybe (fromJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T (findIndex, index, length, lines, null, uncons)
import qualified Data.Text.Read as T (decimal)
import Data.Vector (Vector)
import qualified Data.Vector as V ((!), (!?), fromList, head, length)

data Move a = Move a | TurnL | TurnR
data Dir = R | D | L | U deriving (Enum, Eq, Ord)

readsPath :: (Integral a) => Text -> ([Move a], Text)
readsPath (T.decimal -> Right (n, s)) = let ~(path, s') = readsPath s in (Move n:path, s')
readsPath (T.uncons -> Just ('L', s)) = let ~(path, s') = readsPath s in (TurnL:path, s')
readsPath (T.uncons -> Just ('R', s)) = let ~(path, s') = readsPath s in (TurnR:path, s')
readsPath s = ([], s)

turnRight, turn180, turnLeft :: Dir -> Dir
turnRight d = toEnum $ (fromEnum d + 1) `mod` 4
turn180 d = toEnum $ (fromEnum d + 2) `mod` 4
turnLeft d = toEnum $ (fromEnum d + 3) `mod` 4

step :: (Num a) => Dir -> (a, a) -> (a, a)
step R (x, y) = (x + 1, y)
step D (x, y) = (x, y + 1)
step L (x, y) = (x - 1, y)
step U (x, y) = (x, y - 1)

get :: Vector Text -> (Int, Int) -> Char
get maze (x, y)
  | Just line <- maze V.!? y, inRange (0, T.length line - 1) x = T.index line x
  | otherwise = ' '

mazePerimeter :: Vector Text -> [((Int, Int), Dir)]
mazePerimeter maze
  | Just x0 <- maze V.!? 0 >>= T.findIndex (== '.')
  = let initial:rest = iterate step' ((x0, 0), R) in initial:takeWhile (/= initial) rest where
    step' (p, d)
      | ' ' <- get maze p' = (p, turnRight d)
      | ' ' <- get maze p'' = (p', d)
      | otherwise = (p'', turnLeft d)
      where
        p' = step d p
        p'' = step (turnLeft d) p'
mazePerimeter _ = []

mazeEdges2D, mazeEdges3D :: Vector Text -> Map ((Int, Int), Dir) ((Int, Int), Dir)
mazeEdges2D maze = Map.fromList
  [ ((p, d'), (wrap d' p, d'))
  | x0 <- maybeToList $ maze V.!? 0 >>= T.findIndex (== '.')
  , (p, d) <- mazePerimeter maze
  , let d' = turnLeft d
  ] where
    wrap d (x, y) = fromJust . find ((/= ' ') . get maze) $ case d of
        R -> [(x', y) | x' <- [0..x]]
        D -> [(x, y') | y' <- [0..y]]
        L -> [(x', y) | let line = maze V.! y, x' <- [T.length line - 1, T.length line - 2..x]]
        U -> [(x, y') | y' <- [V.length maze - 1, V.length maze - 2..y]]
mazeEdges3D maze = Map.fromList $ concat
  [ [((p, turnLeft d), (q, turnRight e)), ((q, turnLeft e), (p, turnRight d))]
  | (edge1, edge2) <- joinedEdges
  , ((p, d), (q, e)) <- zip edge1 $ reverse edge2
  ] where
    perimeter = mazePerimeter maze
    sideLength = foldl1' gcd . map length $ groupBy ((==) `on` snd) perimeter
    joinEdges [] = []
    joinEdges edges
      | [] <- joined = error "loop"
      | otherwise = joined ++ joinEdges remaining where
        (joined, remaining) = partitionEithers $ joinEdges' edges
    joinEdges' ((d1, e1):(d2, e2):edges)
      | turnLeft d1 == d2 = Left (e1, e2) : joinEdges' (first turnLeft <$> edges)
    joinEdges' (edge:edges) = Right edge : joinEdges' edges
    joinEdges' [] = []
    joinedEdges = joinEdges [(dir, edge) | edge@((_, dir):_) <- chunksOf sideLength perimeter]

day22 :: (Vector Text -> Map ((Int, Int), Dir) ((Int, Int), Dir)) -> Text -> Int
day22 mazeEdges input = 1000 * (y + 1) + 4 * (x + 1) + fromEnum d where
    (V.fromList -> maze, [_, readsPath -> (path, "")]) = break T.null $ T.lines input
    Just x0 = T.findIndex (== '.') $ V.head maze
    edges = mazeEdges maze
    step' s | Just s' <- edges Map.!? s = s'
    step' (p, d) = (step d p, d)
    go s (Move n) = last . takeWhile ((== '.') . get maze . fst) . take (n + 1) $ iterate step' s
    go (p, d) TurnL = (p, turnLeft d)
    go (p, d) TurnR = (p, turnRight d)
    ((x, y), d) = foldl' go ((x0, 0), R) path

day22a :: Text -> Int
day22a = day22 mazeEdges2D

day22b :: Text -> Int
day22b = day22 mazeEdges3D
