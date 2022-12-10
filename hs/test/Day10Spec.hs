{-# LANGUAGE OverloadedStrings #-}
module Day10Spec (spec) where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day10 (day10a, day10b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "addx 15"
  , "addx -11"
  , "addx 6"
  , "addx -3"
  , "addx 5"
  , "addx -1"
  , "addx -8"
  , "addx 13"
  , "addx 4"
  , "noop"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx -35"
  , "addx 1"
  , "addx 24"
  , "addx -19"
  , "addx 1"
  , "addx 16"
  , "addx -11"
  , "noop"
  , "noop"
  , "addx 21"
  , "addx -15"
  , "noop"
  , "noop"
  , "addx -3"
  , "addx 9"
  , "addx 1"
  , "addx -3"
  , "addx 8"
  , "addx 1"
  , "addx 5"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "addx -36"
  , "noop"
  , "addx 1"
  , "addx 7"
  , "noop"
  , "noop"
  , "noop"
  , "addx 2"
  , "addx 6"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "addx 1"
  , "noop"
  , "noop"
  , "addx 7"
  , "addx 1"
  , "noop"
  , "addx -13"
  , "addx 13"
  , "addx 7"
  , "noop"
  , "addx 1"
  , "addx -33"
  , "noop"
  , "noop"
  , "noop"
  , "addx 2"
  , "noop"
  , "noop"
  , "noop"
  , "addx 8"
  , "noop"
  , "addx -1"
  , "addx 2"
  , "addx 1"
  , "noop"
  , "addx 17"
  , "addx -9"
  , "addx 1"
  , "addx 1"
  , "addx -3"
  , "addx 11"
  , "noop"
  , "noop"
  , "addx 1"
  , "noop"
  , "addx 1"
  , "noop"
  , "noop"
  , "addx -13"
  , "addx -19"
  , "addx 1"
  , "addx 3"
  , "addx 26"
  , "addx -30"
  , "addx 12"
  , "addx -1"
  , "addx 3"
  , "addx 1"
  , "noop"
  , "noop"
  , "noop"
  , "addx -9"
  , "addx 18"
  , "addx 1"
  , "addx 2"
  , "noop"
  , "noop"
  , "addx 9"
  , "noop"
  , "noop"
  , "noop"
  , "addx -1"
  , "addx 2"
  , "addx -37"
  , "addx 1"
  , "addx 3"
  , "noop"
  , "addx 15"
  , "addx -21"
  , "addx 22"
  , "addx -6"
  , "addx 1"
  , "noop"
  , "addx 2"
  , "addx 1"
  , "noop"
  , "addx -10"
  , "noop"
  , "noop"
  , "addx 20"
  , "addx 1"
  , "addx 2"
  , "addx 2"
  , "addx -6"
  , "addx -11"
  , "noop"
  , "noop"
  , "noop"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day10a example `shouldBe` 13140
    describe "part 2" $ do
        it "examples" $ do
            day10b example `shouldBe` intercalate "\n"
              [ "▓▓░░▓▓░░▓▓░░▓▓░░▓▓░░▓▓░░▓▓░░▓▓░░▓▓░░▓▓░░"
              , "▓▓▓░░░▓▓▓░░░▓▓▓░░░▓▓▓░░░▓▓▓░░░▓▓▓░░░▓▓▓░"
              , "▓▓▓▓░░░░▓▓▓▓░░░░▓▓▓▓░░░░▓▓▓▓░░░░▓▓▓▓░░░░"
              , "▓▓▓▓▓░░░░░▓▓▓▓▓░░░░░▓▓▓▓▓░░░░░▓▓▓▓▓░░░░░"
              , "▓▓▓▓▓▓░░░░░░▓▓▓▓▓▓░░░░░░▓▓▓▓▓▓░░░░░░▓▓▓▓"
              , "▓▓▓▓▓▓▓░░░░░░░▓▓▓▓▓▓▓░░░░░░░▓▓▓▓▓▓▓░░░░░"
              ]
