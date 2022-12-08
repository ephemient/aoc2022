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

import Control.Monad ((<=<), when)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (putStrLn, readFile)
import Paths_aoc2022 (getDataFileName)
import System.Environment (getArgs)
import Text.Read (readMaybe)

getDayInput :: Int -> IO Text
getDayInput i = getDataFileName ("day" ++ show i ++ ".txt") >>= TIO.readFile

run :: Int -> (a -> IO ()) -> [Text -> a] -> IO ()
run day showIO funcs = do
    days <- mapMaybe readMaybe <$> getArgs
    when (null days || day `elem` days) $ do
    putStrLn $ "Day " ++ show day
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
