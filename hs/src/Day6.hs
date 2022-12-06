{-|
Module:         Day6
Description:    <https://adventofcode.com/2022/day/6 Day 6: Tuning Trouble>
-}
module Day6 (day6a, day6b) where

import Control.Monad (ap)
import Data.List (findIndex, scanl')
import qualified Data.Set as Set (empty, insert, notMember)
import Data.Text (Text)
import qualified Data.Text as T (length, tails, take, unpack)

day6 :: Int -> Text -> Maybe Int
day6 n = fmap (n +) . findIndex (ok . T.take n) . takeWhile ((>= n) . T.length) . T.tails where
    ok s = and $ zipWith Set.notMember `ap` scanl' (flip Set.insert) Set.empty $ T.unpack s

day6a :: Text -> Maybe Int
day6a = day6 4

day6b :: Text -> Maybe Int
day6b = day6 14
