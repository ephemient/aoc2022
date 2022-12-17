{-# LANGUAGE OverloadedStrings #-}
module Day17Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day17 (day17)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day17 2022 example `shouldBe` 3068
    describe "part 2" $ do
        it "examples" $ do
            day17 1000000000000 example `shouldBe` 1514285714288
