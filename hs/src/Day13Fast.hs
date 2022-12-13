{-|
Module:         Day13Fast
Description:    <https://adventofcode.com/2022/day/13 Day 13: Distress Signal>
-}
{-# LANGUAGE ViewPatterns #-}
module Day13Fast (day13aFast, day13bFast) where

import Data.List (partition, stripPrefix)
import Data.List.Split (chunksOf)
import Data.Text (Text)
import qualified Data.Text as T (lines, null, uncons)
import qualified Data.Text.Read as T (decimal)

data PacketToken a = PacketOpen | PacketClose | PacketInt a deriving (Eq)

tokenize :: (Integral a) => Text -> [PacketToken a]
tokenize (T.decimal -> Right (a, s)) = PacketInt a:tokenize s
tokenize (T.uncons -> Just (a, s))
  | '[' <- a = PacketOpen:tokenize s
  | ']' <- a = PacketClose:tokenize s
  | otherwise = tokenize s
tokenize _ = []

compare' :: (Ord a) => [PacketToken a] -> [PacketToken a] -> Ordering
compare' [] [] = EQ
compare' [] _ = LT
compare' _ [] = GT
compare' (PacketClose:as) (PacketClose:bs) = compare' as bs
compare' (PacketClose:_) _ = LT
compare' _ (PacketClose:_) = GT
compare' (PacketInt a:as) (PacketInt b:bs) = compare a b <> compare' as bs
compare' (PacketInt a:as) (break (/= PacketOpen) -> (length -> n, bs))
  | PacketInt b:bs' <- bs
  = compare a b <> maybe LT (compare' as) (stripPrefix (replicate n PacketClose) bs')
  | otherwise = GT
compare' ~(break (/= PacketOpen) -> (length -> n, as)) (PacketInt b:bs)
  | PacketInt a:as' <- as
  = compare a b <> maybe GT (flip compare' bs) (stripPrefix (replicate n PacketClose) as')
  | otherwise = LT
compare' (a:as) (b:bs) | a == b = compare' as bs

day13aFast :: Text -> Int
day13aFast input = sum
  [ i
  | (i, x:y:_) <- zip [1..] . chunksOf 3 . map tokenize $ T.lines input
  , compare' x y <= EQ
  ]

day13bFast :: Text -> Int
day13bFast input = x * y where
    packets = map tokenize . filter (not . T.null) $ T.lines input
    a = [PacketOpen, PacketOpen, PacketInt 2, PacketClose, PacketClose]
    b = [PacketOpen, PacketOpen, PacketInt 6, PacketClose, PacketClose]
    (belowA, aboveA) = partition (\x -> compare' x a < EQ) packets
    belowB = filter (\x -> compare' x b < EQ) aboveA
    x = 1 + length belowA
    y = 1 + x + length belowB
