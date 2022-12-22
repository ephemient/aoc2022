{-# LANGUAGE OverloadedStrings #-}
module Day22Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day22 (day22a, day22b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "        ...#"
  , "        .#.."
  , "        #..."
  , "        ...."
  , "...#.......#"
  , "........#..."
  , "..#....#...."
  , "..........#."
  , "        ...#...."
  , "        .....#.."
  , "        .#......"
  , "        ......#."
  , ""
  , "10R5L5R10L4R5L5"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day22a example `shouldBe` 6032
    describe "part 2" $ do
        it "examples" $ do
            day22b example `shouldBe` 5031
