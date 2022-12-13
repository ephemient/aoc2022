{-|
Module:         Day13
Description:    <https://adventofcode.com/2022/day/13 Day 13: Distress Signal>
-}
{-# LANGUAGE TypeFamilies #-}
module Day13 (day13a, day13b) where

import Data.List (partition)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, (<|>), between, parse, sepBy, sepEndBy, single, some)
import Text.Megaparsec.Char (eol)
import qualified Text.Megaparsec.Char.Lexer as L (decimal)

data Packet a = Literal a | List [Packet a] deriving (Eq)
instance (Ord a) => Ord (Packet a) where
    compare (Literal x) (Literal y) = compare x y
    compare x@(Literal _) y = compare (List [x]) y
    compare x y@(Literal _) = compare x (List [y])
    compare (List x) (List y) = foldr (<>) (comparing (map $ const ()) x y) $ zipWith compare x y

parser :: (MonadParsec e s m, Token s ~ Char, Integral a) => m (Packet a)
parser = Literal <$> L.decimal <|>
    List <$> between (single '[') (single ']') (parser `sepBy` single ',')

day13a :: Text -> Either (ParseErrorBundle Text Void) Int
day13a input = do
    packets <- parse (parser `sepEndBy` some eol) "day13.txt" input
    pure $ sum [i | (i, [x, y]) <- zip [1..] $ chunksOf 2 packets , x <= y]

day13b :: Text -> Either (ParseErrorBundle Text Void) Int
day13b input = do
    packets <- parse (parser `sepEndBy` some eol) "day13.txt" input
    let a = List [List [Literal 2]]
        b = List [List [Literal 6]]
        (belowA, aboveA) = partition (< a) packets
        belowB = filter (< b) aboveA
        x = 1 + length belowA
        y = 1 + x + length belowB
    pure $ x * y
