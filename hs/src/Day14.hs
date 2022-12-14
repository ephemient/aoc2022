{-|
Module:         Day14
Description:    <https://adventofcode.com/2022/day/14 Day 14: Regolith Reservoir>
-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
module Day14 (day14) where

import Common (readEntire)
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (MArray, newArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Bool (bool)
import Data.Ix (Ix)
import Data.Maybe (fromMaybe)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Semigroup (Max(Max), Min(Min))
import Data.Text (Text)
import qualified Data.Text as T (lines, splitOn)
import qualified Data.Text.Read as T (decimal)

parse :: (Bounded i, Integral i, Ix i, MArray a Bool (ST s)) => Text -> ST s (a (i, i) Bool, i)
parse input = do
    blocks <- newArray
      ( (min minX $ 500 - maxY - 1, min 0 minY)
      , (max maxX $ 500 + maxY + 1, max 0 maxY + 1)
      ) False
    forM_ segments $ \points ->
        forM_ (zip points $ tail points) $ \((x1, y1), (x2, y2)) ->
            forM_ [min y1 y2..max y1 y2] $ \y ->
                forM_ [min x1 x2..max x1 x2] $ \x ->
                    writeArray blocks (x, y) True
    pure (blocks, maxY)
  where
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

fill :: (MArray a Bool (ST s), Ix i, Num i, Show i) => a (i, i) Bool -> i -> ST s (Int, Int)
fill blocks maxY = do
    counterAtMaxY <- newSTRef Nothing
    counter <- newSTRef 0
    let fill' (x, y) = readArray blocks (x, y) >>= flip bool (pure ()) do
            when (y == maxY) $ readSTRef counterAtMaxY >>= maybe
                (readSTRef counter >>= writeSTRef counterAtMaxY . Just) (const $ pure ())
            when (y <= maxY) $ fill' (x, y + 1) >> fill' (x - 1, y + 1) >> fill' (x + 1, y + 1)
            writeArray blocks (x, y) True >> modifySTRef' counter (+ 1)
    fill' (500, 0)
    counterAtMaxY <- readSTRef counterAtMaxY
    counter <- readSTRef counter
    pure (fromMaybe counter counterAtMaxY, counter)

day14 :: Text -> (Int, Int)
day14 input = runST $ parsed >>= uncurry fill where
    parsed :: ST s (STUArray s (Int, Int) Bool, Int)
    parsed = parse input
