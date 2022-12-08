{-# LANGUAGE OverloadedStrings #-}
module Day8Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day8 (day8a, day8b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "30373"
  , "25512"
  , "65332"
  , "33549"
  , "35390"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day8a example `shouldBe` 21
    describe "part 2" $ do
        it "examples" $ do
            day8b example `shouldBe` 8
