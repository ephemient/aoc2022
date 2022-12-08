{-|
Module:         Day8
Description:    <https://adventofcode.com/2022/day/8 Day 8: Treetop Tree House>
-}
module Day8 (day8a, day8b) where

import Control.Monad (ap)
import Data.List (findIndex, maximum, scanl', tails, transpose)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)

day8a :: Text -> Int
day8a input = length $ filter id $ concat both where
    heights = T.unpack <$> T.lines input
    visible = zipWith (>) `ap` scanl' max minBound
    rows = [zipWith (||) (visible row) $ reverse $ visible $ reverse row | row <- heights]
    cols = [zipWith (||) (visible col) $ reverse $ visible $ reverse col | col <- transpose heights]
    both = zipWith (zipWith (||)) rows $ transpose cols

day8b :: Text -> Int
day8b input = maximum $ concat both where
    heights = T.unpack <$> T.lines input
    score row = [maybe (length xs) succ $ findIndex (>= x) xs | (x:xs) <- tails row]
    rows = [zipWith (*) (score row) $ reverse $ score $ reverse row | row <- heights]
    cols = [zipWith (*) (score col) $ reverse $ score $ reverse col | col <- transpose heights]
    both = zipWith (zipWith (*)) rows $ transpose cols
