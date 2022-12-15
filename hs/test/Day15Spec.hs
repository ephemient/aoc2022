{-# LANGUAGE OverloadedStrings #-}
module Day15Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day15 (day15a, day15b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
  , "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
  , "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
  , "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
  , "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
  , "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
  , "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
  , "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
  , "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
  , "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
  , "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
  , "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
  , "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
  , "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day15a 10 example `shouldBe` 26
    describe "part 2" $ do
        it "examples" $ do
            day15b 20 example `shouldBe` 56000011
