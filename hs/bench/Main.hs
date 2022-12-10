module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, env, nf)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (readFile)
import Day1 (day1a, day1b)
import Day2 (day2a, day2b)
import Day3 (day3a, day3b)
import Day4 (day4a, day4b)
import Day5 (day5a, day5b)
import Day6 (day6a, day6b)
import Day7 (day7a, day7b)
import Day8 (day8a, day8b)
import Day9 (day9a, day9b)
import Day10 (day10a, day10b)
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
  , env (getDayInput 5) $ \input -> bgroup "Day 5"
      [ bench "part 1" $ nf day5a input
      , bench "part 2" $ nf day5b input
      ]
  , env (getDayInput 6) $ \input -> bgroup "Day 6"
      [ bench "part 1" $ nf day6a input
      , bench "part 2" $ nf day6b input
      ]
  , env (getDayInput 7) $ \input -> bgroup "Day 7"
      [ bench "part 1" $ nf day7a input
      , bench "part 2" $ nf day7b input
      ]
  , env (getDayInput 8) $ \input -> bgroup "Day 8"
      [ bench "part 1" $ nf day8a input
      , bench "part 2" $ nf day8b input
      ]
  , env (getDayInput 9) $ \input -> bgroup "Day 9"
      [ bench "part 1" $ nf day9a input
      , bench "part 2" $ nf day9b input
      ]
  , env (getDayInput 10) $ \input -> bgroup "Day 10"
      [ bench "part 1" $ nf day10a input
      , bench "part 2" $ nf day10b input
      ]
  ]
