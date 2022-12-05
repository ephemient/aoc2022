{-# LANGUAGE OverloadedStrings #-}
module Day5Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day5 (day5a, day5b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "    [D]    "
  , "[N] [C]    "
  , "[Z] [M] [P]"
  , " 1   2   3 "
  , ""
  , "move 1 from 2 to 1"
  , "move 3 from 1 to 3"
  , "move 2 from 2 to 1"
  , "move 1 from 1 to 2"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day5a example `shouldBe` "CMZ"
    describe "part 2" $ do
        it "examples" $ do
            day5b example `shouldBe` "MCD"
