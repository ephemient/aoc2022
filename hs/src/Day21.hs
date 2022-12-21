{-|
Module:         Day21
Description:    <https://adventofcode.com/2022/day/21 Day 21: Monkey Math>
-}
{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Day21 (day21a, day21b) where

import Data.Char (isAlphaNum)
import qualified Data.Map as Map ((!), fromList, insert)
import Data.Ratio ((%), denominator, numerator)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, (<|>), choice, parse, sepEndBy, takeWhile1P)
import Text.Megaparsec.Char (eol, string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal)

data Expr a b = Literal a | b :+ b | b :- b | b :* b | b :/ b

parser :: (Integral a, MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m [(Tokens s, Expr a (Tokens s))]
parser = parseLine `sepEndBy` eol where
    parseLine = (,) <$> name <* string ": " <*> expr
    expr = Literal <$> L.decimal <|> flip ($) <$> name <*> op <*> name
    op = (:+) <$ string " + " <|>
        (:-) <$ string " - " <|>
        (:*) <$ string " * " <|>
        (:/) <$ string " / "
    name = takeWhile1P Nothing isAlphaNum

day21a :: Text -> Either (ParseErrorBundle Text Void) Int
day21a input = do
    monkeys <- Map.fromList <$> parse parser "day21.txt" input
    let monkeys' = eval <$> monkeys
        eval (Literal a) = a
        eval (x :+ y) = monkeys' Map.! x + monkeys' Map.! y
        eval (x :- y) = monkeys' Map.! x - monkeys' Map.! y
        eval (x :* y) = monkeys' Map.! x * monkeys' Map.! y
        eval (x :/ y) = monkeys' Map.! x `div` monkeys' Map.! y
    pure $ monkeys' Map.! "root"

day21b :: Text -> Either (ParseErrorBundle Text Void) Int
day21b input = do
    monkeys <- Map.fromList <$> parse parser "day21.txt" input
    let (lhs, rhs) = case monkeys Map.! "root" of
            lhs :+ rhs -> (lhs, rhs)
            lhs :- rhs -> (lhs, rhs)
            lhs :* rhs -> (lhs, rhs)
            lhs :/ rhs -> (lhs, rhs)
        monkeys' = Map.insert "humn" (1, 0) $ eval <$> monkeys
        eval (Literal a) = (0, a % 1)
        eval (x :+ y) = (monkeys' Map.! x) +: (monkeys' Map.! y)
        eval (x :- y) = (monkeys' Map.! x) -: (monkeys' Map.! y)
        eval (x :* y) = (monkeys' Map.! x) *: (monkeys' Map.! y)
        eval (x :/ y) = (monkeys' Map.! x) /: (monkeys' Map.! y)
        (a, b) +: (c, d) = (a + c, b + d)
        (a, b) -: (c, d) = (a - c, b - d)
        (0, b) *: (c, d) = (b * c, b * d)
        (a, b) *: (0, d) = (a * d, b * d)
        (a, b) /: (0, d) = (a / d, b / d)
        (m, b) = monkeys' Map.! lhs
        (n, c) = monkeys' Map.! rhs
        x = (c - b) / (m - n)
    pure $ if denominator x == 1 then numerator x else error "non-integral"
