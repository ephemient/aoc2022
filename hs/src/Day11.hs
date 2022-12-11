{-|
Module:         Day11
Description:    <https://adventofcode.com/2022/day/11 Day 11: Monkey in the Middle>
-}
{-# LANGUAGE NoFieldSelectors, OverloadedRecordDot, OverloadedStrings, TypeFamilies #-}
module Day11 (day11a, day11b) where

import Control.Arrow (second)
import Control.Monad (join)
import Data.Char (isAlphaNum)
import Data.List (foldl', mapAccumL, sortOn)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map ((!), fromListWith, insert, insertWith)
import Data.Ord (Down(Down))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq ((><), empty, fromList, length, singleton)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, between, choice, chunk, parse, sepBy, sepEndBy, single, some, takeWhile1P)
import Text.Megaparsec.Char (eol, space)
import qualified Text.Megaparsec.Char.Lexer as L (decimal)

data Monkey k v = Monkey {operation :: v -> v , test :: v , ifTrue :: k , ifFalse :: k}

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m [(Tokens s, ([a], Monkey (Tokens s) a))]
parser = monkey `sepEndBy` some eol where
    monkey = (,) <$> between (chunk "Monkey ") (single ':') identifier <*> do
        (,) <$> (space >> chunk "Starting items: " >> L.decimal `sepBy` chunk ", ") <*> do
            Monkey <$> (space >> chunk "Operation: " >> operation) <*>
                (space >> chunk "Test: divisible by " >> L.decimal) <*>
                (space >> chunk "If true: throw to monkey " >> identifier) <*>
                (space >> chunk "If false: throw to monkey " >> identifier)
    identifier = takeWhile1P Nothing isAlphaNum
    operation = chunk "new = old " >> choice
        [join (*) <$ chunk "* old", chunk "* " >> (*) <$> L.decimal, chunk "+ " >> (+) <$> L.decimal]

go :: (Ord k, Integral v) => Map k (Seq v) -> (k, Monkey k v) -> (Map k (Seq v), Int)
go items (name, monkey) = (foldl' send (Map.insert name Seq.empty items) thrown, Seq.length thrown) where
    thrown = monkey.operation <$> items Map.! name
    send items' item = Map.insertWith (flip (Seq.><))
        (if item `mod` monkey.test == 0 then monkey.ifTrue else monkey.ifFalse) (Seq.singleton item) items'
        

day11a :: Text -> Either (ParseErrorBundle Text Void) Int
day11a input = do
    parsed <- parse parser "day11.txt" input
    let size = length parsed
        (_, counts) = mapAccumL go (Map.fromListWith (Seq.><) $ second (Seq.fromList . fst) <$> parsed) $ cycle
          [ (name, monkey {operation = (`div` 3) . operation})
          | (name, (_, monkey@Monkey {operation})) <- parsed
          ]
    pure $ product $ take 2 $ sortOn Down $ foldl' (zipWith (+)) (replicate size 0) $ take 20 $ chunksOf size counts

day11b :: Text -> Either (ParseErrorBundle Text Void) Int
day11b input = do
    parsed <- parse parser "day11.txt" input
    let size = length parsed
        base = foldl' lcm 1 $ (.test) . snd . snd <$> parsed
        (_, counts) = mapAccumL go (Map.fromListWith (Seq.><) $ second (Seq.fromList . fst) <$> parsed) $ cycle
          [ (name, monkey {operation = (`mod` base) . operation})
          | (name, (_, monkey@Monkey {operation})) <- parsed
          ]
    pure $ product $ take 2 $ sortOn Down $ foldl' (zipWith (+)) (replicate size 0) $ take 10000 $ chunksOf size counts
