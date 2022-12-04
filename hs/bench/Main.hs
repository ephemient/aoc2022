module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, env, nf)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (readFile)
import Day1 (day1a, day1b)
import Day2 (day2a, day2b)
import Day3 (day3a, day3b)
import Day4 (day4a, day4b)
import Paths_aoc2022 (getDataFileName)

getDayInput :: Int -> IO Text
getDayInput i = getDataFileName ("day" ++ show i ++ ".txt") >>= TIO.readFile

main :: IO ()
main = defaultMain
  [ env (getDayInput 1) $ \input -> bgroup "Day 1"
      [ bench "part 1" $ nf day1a input
      , bench "part 2" $ nf day1b input
      ]
  , env (getDayInput 2) $ \input -> bgroup "Day 2"
      [ bench "part 1" $ nf day2a input
      , bench "part 2" $ nf day2b input
      ]
  , env (getDayInput 3) $ \input -> bgroup "Day 3"
      [ bench "part 1" $ nf day3a input
      , bench "part 2" $ nf day3b input
      ]
  , env (getDayInput 4) $ \input -> bgroup "Day 4"
      [ bench "part 1" $ nf day4a input
      , bench "part 2" $ nf day4b input
      ]
  ]
