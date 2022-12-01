{-# LANGUAGE OverloadedStrings #-}
module Day1Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day1 (day1a, day1b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "1000"
  , "2000"
  , "3000"
  , ""
  , "4000"
  , ""
  , "5000"
  , "6000"
  , ""
  , "7000"
  , "8000"
  , "9000"
  , ""
  , "10000"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day1a example `shouldBe` 24000
    describe "part 2" $ do
        it "examples" $ do
            day1b example `shouldBe` 45000
