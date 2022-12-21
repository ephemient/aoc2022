{-# LANGUAGE OverloadedStrings #-}
module Day21Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day21 (day21a, day21b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "root: pppw + sjmn"
  , "dbpl: 5"
  , "cczh: sllz + lgvd"
  , "zczc: 2"
  , "ptdq: humn - dvpt"
  , "dvpt: 3"
  , "lfqf: 4"
  , "humn: 5"
  , "ljgn: 2"
  , "sjmn: drzm * dbpl"
  , "sllz: 4"
  , "pppw: cczh / lfqf"
  , "lgvd: ljgn * ptdq"
  , "drzm: hmdt - zczc"
  , "hmdt: 32"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day21a example `shouldBe` Right 152
    describe "part 2" $ do
        it "examples" $ do
            day21b example `shouldBe` Right 301
