module Main (main) where

import Control.Arrow ((>>>))
import Criterion.Main (bench, bgroup, defaultMain, env, envWithCleanup, nf)
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
import Day11 (day11a, day11b)
import Day12 (day12)
import Day13 (day13a, day13b)
import Day13Fast (day13aFast, day13bFast)
import Day14 (day14)
import Day15 (day15a, day15b)
import Day16 (day16)
import Day17 (day17)
import Day18 (day18a, day18b)
import Day19 (day19a, day19b)
import Day20 (day20a, day20b)
import Paths_aoc2022 (getDataFileName)
import System.Environment.Blank (getEnv, setEnv, unsetEnv)

setTrace :: String -> IO (Maybe String)
setTrace value = getEnv "TRACE" <* setEnv "TRACE" value True

unsetTrace :: Maybe String -> IO ()
unsetTrace = maybe (unsetEnv "TRACE") (setEnv "TRACE" `flip` True)

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
  , env (getDayInput 11) $ \input -> bgroup "Day 11"
      [ bench "part 1" $ nf day11a input
      , bench "part 2" $ nf day11b input
      ]
  , env (getDayInput 12) $ \input -> bgroup "Day 12"
      [ bench "both" $ nf day12 input
      ]
  , env (getDayInput 13) $ \input -> bgroup "Day 13"
      [ bench "part 1" $ nf day13a input
      , bench "part 1 (fast)" $ nf day13aFast input
      , bench "part 2" $ nf day13b input
      , bench "part 2 (fast)" $ nf day13bFast input
      ]
  , env (getDayInput 14) $ \input -> bgroup "Day 14"
      [ bench "both" $ nf day14 input
      ]
  , env (getDayInput 15) $ \input -> bgroup "Day 15"
      [ bench "part 1" $ nf (day15a 2000000) input
      , bench "part 2" $ nf (day15b 4000000) input
      ]
  , envWithCleanup ((,) <$> getDayInput 16 <*> setTrace "0")
        (unsetTrace . snd) $ fst >>> \input -> bgroup "Day 16"
          [ bench "part 1" $ nf (day16 1 30) input
          , bench "part 2" $ nf (day16 2 26) input
          ]
  , env (getDayInput 17) $ \input -> bgroup "Day 17"
      [ bench "part 1" $ nf (day17 2022) input
      , bench "part 2" $ nf (day17 1000000000000) input
      ]
  , env (getDayInput 18) $ \input -> bgroup "Day 18"
      [ bench "part 1" $ nf day18a input
      , bench "part 2" $ nf day18b input
      ]
  , envWithCleanup ((,) <$> getDayInput 19 <*> setTrace "0")
        (unsetTrace . snd) $ fst >>> \input -> bgroup "Day 19"
          [ bench "part 1" $ nf day19a input
          , bench "part 2" $ nf day19b input
          ]
  , env (getDayInput 20) $ \input -> bgroup "Day 20"
      [ bench "part 1" $ nf day20a input
      , bench "part 2" $ nf day20b input
      ]
  ]
