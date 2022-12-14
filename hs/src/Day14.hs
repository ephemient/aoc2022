{-|
Module:         Day14
Description:    <https://adventofcode.com/2022/day/14 Day 14: Regolith Reservoir>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day14 (day14a, day14b) where

import Common (readEntire)
import Control.Monad ((>=>))
import Data.List (foldl', unfoldr)
import Data.Semigroup (Max(Max))
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, fromList, member, notMember, toList)
import Data.Text (Text)
import qualified Data.Text as T (lines, splitOn)
import qualified Data.Text.Read as T (decimal)

parse :: (Integral a) => Text -> Set (a, a)
parse = Set.fromList . (T.lines >=> parseLine) where
    parseLine line = concat $ zipWith segment points (tail points) where
        points =
          [ (x', y')
          | [x, y] <- T.splitOn "," <$> T.splitOn " -> " line
          , x' <- either fail pure $ readEntire T.decimal x
          , y' <- either fail pure $ readEntire T.decimal y
          ]
        segment (x1, y1) (x2, y2)
          | x1 == x2 = [(x1, y) | y <- [min y1 y2..max y1 y2]]
          | y1 == y2 = [(x, y1) | x <- [min x1 x2..max x1 x2]]
          | otherwise = []

fall :: (Num a, Ord a) => a -> Set (a, a) -> Maybe (Either a (a, a))
fall maxY blocks
  | (500, 0) `Set.member` blocks = Nothing
  | otherwise = pure $ fall' (500, 0) where
    fall' (x, y)
      | y > maxY = Left x
      | (x, y + 1) `Set.notMember` blocks = fall' (x, y + 1)
      | (x - 1, y + 1) `Set.notMember` blocks = fall' (x - 1, y + 1)
      | (x + 1, y + 1) `Set.notMember` blocks = fall' (x + 1, y + 1)
      | otherwise = Right (x, y)

day14a :: Text -> Int
day14a input = length $ unfoldr f blocks where
    blocks = parse @Int input
    Max maxY = foldMap (Max . snd) blocks
    f blocks' = fall maxY blocks' >>= either (const Nothing) (Just . ((),) . (`Set.insert` blocks'))

day14b :: Text -> Int
day14b input = length $ unfoldr f blocks where
    blocks = parse @Int input
    Max maxY = succ <$> foldMap (Max . snd) blocks
    f blocks' = ((),) . (`Set.insert` blocks') . either (, maxY) id <$> fall maxY blocks'
