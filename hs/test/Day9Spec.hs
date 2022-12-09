{-# LANGUAGE OverloadedStrings #-}
module Day9Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day9 (day9a, day9b)
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2 :: Text
example1 = T.unlines
  [ "R 4"
  , "U 4"
  , "L 3"
  , "D 1"
  , "R 4"
  , "D 1"
  , "L 5"
  , "R 2"
  ]
example2 = T.unlines
  [ "R 5"
  , "U 8"
  , "L 8"
  , "D 3"
  , "R 17"
  , "D 10"
  , "L 25"
  , "U 20"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day9a example1 `shouldBe` 13
    describe "part 2" $ do
        it "examples" $ do
            day9b example1 `shouldBe` 1
            day9b example2 `shouldBe` 36
