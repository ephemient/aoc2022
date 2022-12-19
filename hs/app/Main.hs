{-# LANGUAGE NondecreasingIndentation #-}
module Main (main) where

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
import Day19 (day19a)

import Control.Monad ((<=<), ap, when)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (putStrLn, readFile)
import Paths_aoc2022 (getDataFileName)
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)
import Text.Read (readMaybe)

getDayInput :: Int -> IO Text
getDayInput i = getDataFileName ("day" ++ show i ++ ".txt") >>= TIO.readFile

run :: Int -> (a -> IO ()) -> [Text -> a] -> IO ()
run = run' `ap` show

run' :: Int -> String -> (a -> IO ()) -> [Text -> a] -> IO ()
run' day name showIO funcs = do
    args <- getArgs
    when (null args || name `elem` args) $ do
    putStrLn $ "Day " ++ name
    contents <- getDayInput day
    mapM_ (showIO . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = do
    run 1 print [day1a, day1b]
    run 2 print [day2a, day2b]
    run 3 print [day3a, day3b]
    run 4 (either fail print) [day4a, day4b]
    run 5 TIO.putStrLn [day5a, day5b]
    run 6 (maybe (fail "(⊥)") print) [day6a, day6b]
    run 7 print [day7a, day7b]
    run 8 print [day8a, day8b]
    run 9 print [day9a, day9b]
    run 10 putStrLn [show . day10a, day10b]
    run 11 (either (fail . errorBundlePretty) print) [day11a, day11b]
    run 12 (uncurry ((>>) `on` maybe (fail "(⊥)") print)) [day12]
    run 13 (either (fail . errorBundlePretty) print) [day13a, day13b]
    run' 13 "13Fast" print [day13aFast, day13bFast]
    run 14 (uncurry ((>>) `on` print)) [day14]
    run 15 print [day15a 2000000, day15b 4000000]
    run 16 (either (fail . errorBundlePretty) print) [day16 1 30, day16 2 26]
    run 17 print [day17 2022, day17 1000000000000]
    run 18 print [day18a, day18b]
    run 19 print [day19a]
