{-|
Module:         Day14
Description:    <https://adventofcode.com/2022/day/14 Day 14: Regolith Reservoir>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day14 (day14a, day14b) where

import Common (readEntire)
import Control.Monad (forM_)
import Data.Array.IArray (IArray, Ix, (!), (//))
import Data.Array.MArray (newArray, writeArray)
import Data.Array.ST (runSTUArray)
import Data.Array.Unboxed (UArray)
import Data.List (unfoldr)
import Data.Semigroup (Max(Max), Min(Min))
import Data.Text (Text)
import qualified Data.Text as T (lines, splitOn)
import qualified Data.Text.Read as T (decimal)

parse :: (Bounded i, Integral i, Ix i) => Text -> (UArray (i, i) Bool, i)
parse input = (blocks, maxY) where
    segments =
      [ [ (x', y')
        | [x, y] <- T.splitOn "," <$> T.splitOn " -> " line
        , x' <- either fail pure $ readEntire T.decimal x
        , y' <- either fail pure $ readEntire T.decimal y
        ]
      | line <- T.lines input
      ]
    f (x, y) = (Min x, Max x, Min y, Max y)
    (Min minX, Max maxX, Min minY, Max maxY) = foldMap f $ concat segments
    blocks = runSTUArray $ do
        a <- newArray
          ( (min minX $ 500 - maxY - 1, min 0 minY)
          , (max maxX $ 500 + maxY + 1, max 0 maxY + 1)
          ) False
        forM_ segments $ \points ->
            forM_ (zip points $ tail points) $ \((x1, y1), (x2, y2)) ->
                forM_ [min y1 y2..max y1 y2] $ \y ->
                    forM_ [min x1 x2..max x1 x2] $ \x ->
                        writeArray a (x, y) True
        pure a

fall :: (IArray a Bool, Ix i, Num i) => i -> a (i, i) Bool -> Maybe (Either i (i, i))
fall maxY blocks
  | blocks ! (500, 0) = Nothing
  | otherwise = pure $ fall' (500, 0) where
    fall' (x, y)
      | y >= maxY = Left x
      | not $ blocks ! (x, y + 1) = fall' (x, y + 1)
      | not $ blocks ! (x - 1, y + 1) = fall' (x - 1, y + 1)
      | not $ blocks ! (x + 1, y + 1) = fall' (x + 1, y + 1)
      | otherwise = Right (x, y)

day14a :: Text -> Int
day14a input = length $ unfoldr f blocks where
    (blocks, maxY) = parse @Int input
    f blocks' = fall maxY blocks' >>= either (const Nothing) (g blocks')
    g blocks' point = Just ((), blocks' // [(point, True)])

day14b :: Text -> Int
day14b input = length $ unfoldr f blocks where
    (blocks, maxY) = parse @Int input
    f blocks' = g blocks' . either (, maxY + 1) id <$> fall (maxY + 1) blocks'
    g blocks' point = ((), blocks' // [(point, True)])
