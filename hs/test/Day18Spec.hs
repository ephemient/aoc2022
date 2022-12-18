{-# LANGUAGE OverloadedStrings #-}
module Day18Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day18 (day18a, day18b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "2,2,2"
  , "1,2,2"
  , "3,2,2"
  , "2,1,2"
  , "2,3,2"
  , "2,2,1"
  , "2,2,3"
  , "2,2,4"
  , "2,2,6"
  , "1,2,5"
  , "3,2,5"
  , "2,1,5"
  , "2,3,5"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day18a example `shouldBe` 64
    describe "part 2" $ do
        it "examples" $ do
            day18b example `shouldBe` 58
