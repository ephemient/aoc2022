{-|
Module:         Day4
Description:    <https://adventofcode.com/2022/day/4 Day 4: Camp Cleanup>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day4 (day4a, day4b) where

import Common (count)
import Control.Monad (ap)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T (lines, stripPrefix)
import qualified Data.Text.Read as T (decimal)

parse :: (Integral a) => Text -> Either String (a, a, a, a)
parse line = do
    (a, line) <- T.decimal line
    (b, line) <- T.decimal $ fromMaybe `ap` T.stripPrefix "-" $ line
    (c, line) <- T.decimal $ fromMaybe `ap` T.stripPrefix "," $ line
    (d, line) <- T.decimal $ fromMaybe `ap` T.stripPrefix "-" $ line
    pure (a, b, c, d)

day4a :: Text -> Either String Int
day4a input = fmap (count ok) . sequence $ parse <$> T.lines input where
    ok (a, b, c, d) = a >= c && b <= d || a <= c && b >= d

day4b :: Text -> Either String Int
day4b input = fmap (count ok) . sequence $ parse <$> T.lines input where
    ok (a, b, c, d) = a <= d && b >= c
