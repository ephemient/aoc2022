{-# LANGUAGE OverloadedStrings #-}
module Day6Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day6 (day6a, day6b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day6a "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` Just 7
            day6a "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` Just 5
            day6a "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` Just 6
            day6a "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` Just 10
            day6a "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` Just 11
    describe "part 2" $ do
        it "examples" $ do
            day6b "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` Just 19
            day6b "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` Just 23
            day6b "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` Just 23
            day6b "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` Just 29
            day6b "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` Just 26
