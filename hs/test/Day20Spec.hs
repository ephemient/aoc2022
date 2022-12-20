{-# LANGUAGE OverloadedStrings #-}
module Day20Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day20 (day20a, day20b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines ["1", "2", "-3", "3", "-2", "0", "4"]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day20a example `shouldBe` Right 3
    describe "part 2" $ do
        it "examples" $ do
            day20b example `shouldBe` Right 1623178306
