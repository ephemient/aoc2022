{-# LANGUAGE OverloadedStrings #-}
module Day24Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day24 (day24a, day24b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "#.######"
  , "#>>.<^<#"
  , "#.<..<<#"
  , "#>v.><>#"
  , "#<^v^^>#"
  , "######.#"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day24a example `shouldBe` Just 18
    describe "part 2" $ do
        it "examples" $ do
            day24b example `shouldBe` Just 54
