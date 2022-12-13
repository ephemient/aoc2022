{-# LANGUAGE OverloadedStrings #-}
module Day13Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day13 (day13a, day13b)
import Day13Fast (day13aFast, day13bFast)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "[1,1,3,1,1]"
  , "[1,1,5,1,1]"
  , ""
  , "[[1],[2,3,4]]"
  , "[[1],4]"
  , ""
  , "[9]"
  , "[[8,7,6]]"
  , ""
  , "[[4,4],4,4]"
  , "[[4,4],4,4,4]"
  , ""
  , "[7,7,7,7]"
  , "[7,7,7]"
  , ""
  , "[]"
  , "[3]"
  , ""
  , "[[[]]]"
  , "[[]]"
  , ""
  , "[1,[2,[3,[4,[5,6,7]]]],8,9]"
  , "[1,[2,[3,[4,[5,6,0]]]],8,9]"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day13a example `shouldBe` Right 13
        it "examples (fast)" $ do
            day13aFast example `shouldBe` 13
    describe "part 2" $ do
        it "examples" $ do
            day13b example `shouldBe` Right 140
        it "examples (fast)" $ do
            day13bFast example `shouldBe` 140
