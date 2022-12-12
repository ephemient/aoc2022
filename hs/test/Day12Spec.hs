{-# LANGUAGE OverloadedStrings #-}
module Day12Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day12 (day12a, day12b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "Sabqponm"
  , "abcryxxl"
  , "accszExk"
  , "acctuvwj"
  , "abdefghi"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day12a example `shouldBe` Just 31
        it "examples" $ do
            day12b example `shouldBe` Just 29
