{-# LANGUAGE OverloadedStrings #-}
module Day25Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day25 (day25)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "1=-0-2"
  , "12111"
  , "2=0="
  , "21"
  , "2=01"
  , "111"
  , "20012"
  , "112"
  , "1=-1="
  , "1-12"
  , "12"
  , "1="
  , "122"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day25 example `shouldBe` "2=-1=0"
