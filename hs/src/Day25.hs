{-|
Module:         Day25
Description:    <https://adventofcode.com/2022/day/25 Day 25: Full of Hot Air>
-}
module Day25 (day25) where

import Data.Char (digitToInt, intToDigit)
import Data.Text (Text)
import qualified Data.Text as T (foldl', lines, unfoldr, reverse)

day25 :: Text -> Text
day25 = T.reverse . T.unfoldr g . sum . map (T.foldl' f 0) . T.lines where
    f k '=' = 5 * k - 2
    f k '-' = 5 * k - 1
    f k c = 5 * k + digitToInt c
    g 0 = Nothing
    g n = Just $ case n `divMod` 5 of
        (q, 3) -> ('=', q + 1)
        (q, 4) -> ('-', q + 1)
        (q, r) -> (intToDigit r, q)
