{-|
Module:         Day5
Description:    <https://adventofcode.com/2022/day/5 Day 5: Supply Stacks>
-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Day5 (day5a, day5b) where

import Data.Char (isDigit)
import Data.List (foldl')
import Data.List.Split (splitWhen)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T (append, lines, null, reverse, splitAt, stripStart, transpose, unpack, unsnoc, words)
import qualified Data.Text.Read as T (decimal)

day5 :: (Text -> Text) -> Text -> Text
day5 f input = head . T.transpose . Map.elems $ foldl' go stacks0 moves where
    [top, bottom] = splitWhen T.null $ T.lines input
    stack (T.unsnoc -> Just (s, x)) | isDigit x = Just (x, T.stripStart s)
    stack _ = Nothing
    stacks0 = Map.fromList . mapMaybe stack $ T.transpose top
    move ["move", T.decimal -> Right (num, ""), "from", T.unpack -> [x], "to", T.unpack -> [y]]
      | isDigit x, isDigit y
      = Just (num, x, y)
    move _ = Nothing
    moves = mapMaybe (move . T.words) bottom
    go stacks (num, x, y) =
        let (a, b) = T.splitAt num $ stacks Map.! x
        in Map.update (Just . T.append (f a)) y $ Map.insert x b stacks

day5a :: Text -> Text
day5a = day5 T.reverse

day5b :: Text -> Text
day5b = day5 id
