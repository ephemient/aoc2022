{-# LANGUAGE OverloadedStrings #-}
module Day3Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day3 (day3a, day3b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "vJrwpWtwJgWrhcsFMMfFFhFp"
  , "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
  , "PmmdzqPrVvPwwTWBwg"
  , "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
  , "ttgJtRGJQctTZtZT"
  , "CrZsJsPPZsGzwwsLwLmpwMDw"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day3a example `shouldBe` 157
    describe "part 2" $ do
        it "examples" $ do
            day3b example `shouldBe` 70
