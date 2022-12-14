{-# LANGUAGE OverloadedStrings #-}
module Day14Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day14 (day14)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "498,4 -> 498,6 -> 496,6"
  , "503,4 -> 502,4 -> 502,9 -> 494,9"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            fst (day14 example) `shouldBe` 24
    describe "part 2" $ do
        it "examples" $ do
            snd (day14 example) `shouldBe` 93
