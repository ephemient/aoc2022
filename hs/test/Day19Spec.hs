{-# LANGUAGE OverloadedStrings #-}
module Day19Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day19 (day19a)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "Blueprint 1:"
  , "  Each ore robot costs 4 ore."
  , "  Each clay robot costs 2 ore."
  , "  Each obsidian robot costs 3 ore and 14 clay."
  , "  Each geode robot costs 2 ore and 7 obsidian."
  , ""
  , "Blueprint 2:"
  , "  Each ore robot costs 2 ore."
  , "  Each clay robot costs 3 ore."
  , "  Each obsidian robot costs 3 ore and 8 clay."
  , "  Each geode robot costs 3 ore and 12 obsidian."
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day19a example `shouldBe` Right 33
