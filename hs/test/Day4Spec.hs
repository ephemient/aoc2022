{-# LANGUAGE OverloadedStrings #-}
module Day4Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day4 (day4a, day4b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "2-4,6-8"
  , "2-3,4-5"
  , "5-7,7-9"
  , "2-8,3-7"
  , "6-6,4-6"
  , "2-6,4-8"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day4a example `shouldBe` Right 2
    describe "part 2" $ do
        it "examples" $ do
            day4b example `shouldBe` Right 4
