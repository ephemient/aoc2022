{-# LANGUAGE OverloadedStrings #-}
module Day2Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day2 (day2a, day2b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "A Y"
  , "B X"
  , "C Z"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day2a example `shouldBe` 15
    describe "part 2" $ do
        it "examples" $ do
            day2b example `shouldBe` 12
