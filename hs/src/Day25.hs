{-|
Module:         Day25
Description:    <https://adventofcode.com/2022/day/25 Day 25: Full of Hot Air>
-}
{-# LANGUAGE MultiWayIf #-}
module Day25 (day25) where

import Data.Char (digitToInt, intToDigit)
import Data.Text (Text)
import qualified Data.Text as T (foldl', lines, unfoldr, reverse)

day25 :: Text -> Text
day25 = T.reverse . T.unfoldr g . sum . map (T.foldl' f 0) . T.lines where
    f k c = 5 * k + if | '=' <- c -> -2 | '-' <- c -> -1 | otherwise -> digitToInt c
    g 0 = Nothing
    g n = Just (c, q) where
        (q, r) = (n + 2) `divMod` 5
        c | 0 <- r = '=' | 1 <- r = '-' | otherwise = intToDigit $ r - 2
