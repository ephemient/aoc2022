{-|
Module:         Day7
Description:    <https://adventofcode.com/2022/day/7 Day 7: No Space Left On Device>
-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Day7 (day7a, day7b) where

import Data.List (find, foldl', sort, tails)
import Data.Map (Map)
import qualified Data.Map as Map ((!), elems, insertWith, singleton)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T (lines, stripPrefix)
import qualified Data.Text.Read as T (decimal)

parse :: (Integral a) => Text -> Map [Text] a
parse = snd . foldl' go ([], Map.singleton [] 0) . T.lines where
    go (_, dirs) "$ cd /" = ([], dirs)
    go (_:cwd, dirs) "$ cd .." = (cwd, dirs)
    go (cwd, dirs) (T.stripPrefix "$ cd " -> Just dir) = (dir:cwd, dirs)
    go (cwd, dirs) (T.stripPrefix "dir " -> Just dir) = (cwd, Map.insertWith (const id) (dir:cwd) 0 dirs)
    go (cwd, dirs) (T.decimal -> Right (size, _)) =
        (cwd, foldl' (flip $ Map.insertWith (+) `flip` size) dirs $ tails cwd)
    go state _ = state

day7a :: Text -> Int
day7a = sum . filter (<= 100000) . Map.elems . parse

day7b :: Text -> Int
day7b input = fromJust . find ok . sort $ Map.elems dirs where
    dirs = parse input
    total = dirs Map.! []
    ok x = 70000000 - (total - x) >= 30000000
