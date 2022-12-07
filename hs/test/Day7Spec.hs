{-# LANGUAGE OverloadedStrings #-}
module Day7Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day7 (day7a, day7b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "$ cd /"
  , "$ ls"
  , "dir a"
  , "14848514 b.txt"
  , "8504156 c.dat"
  , "dir d"
  , "$ cd a"
  , "$ ls"
  , "dir e"
  , "29116 f"
  , "2557 g"
  , "62596 h.lst"
  , "$ cd e"
  , "$ ls"
  , "584 i"
  , "$ cd .."
  , "$ cd .."
  , "$ cd d"
  , "$ ls"
  , "4060174 j"
  , "8033020 d.log"
  , "5626152 d.ext"
  , "7214296 k"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day7a example `shouldBe` 95437
    describe "part 2" $ do
        it "examples" $ do
            day7b example `shouldBe` 24933642
