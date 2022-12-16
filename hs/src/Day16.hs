{-|
Module:         Day16
Description:    <https://adventofcode.com/2022/day/16 Day 16: Proboscidea Volcanium>
-}
{-# LANGUAGE OverloadedStrings, TypeFamilies, ViewPatterns #-}
module Day16 (day16a) where

import Data.Char (isAlphaNum)
import qualified Data.Heap as Heap (FstMaxPolicy, insert, singleton, view)
import Data.List (foldl')
import qualified Data.Map as Map ((!), fromList)
import qualified Data.Set as Set (empty, insert, member, notMember)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace (traceShow)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, (<|>), parse, sepEndBy, sepEndBy1, takeWhile1P)
import Text.Megaparsec.Char (eol, string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Integral a) => m [(Tokens s, (a, [Tokens s]))]
parser = parseLine `sepEndBy` eol where
    parseLine = do
        string "Valve "
        src <- name
        string " has flow rate="
        w <- L.decimal
        string "; tunnel leads to valve " <|> string "; tunnels lead to valves "
        dsts <- name `sepEndBy1` string ", "
        pure (src, (w, dsts))
    name = takeWhile1P Nothing isAlphaNum

search :: (Ord a, Ord b) => (a -> [(b, a)]) -> (b, a) -> [a]
search next = search' Set.empty . Heap.singleton @Heap.FstMaxPolicy where
    search' seen (Heap.view -> Just ((b, a), heap))
      | Set.member a seen = search' seen heap
      | otherwise = a : search' seen' heap' where
            seen' = Set.insert a seen
            heap' = foldl' (flip Heap.insert) heap $ filter (flip Set.notMember seen' . snd) $ next a
    search' _ _ = []

day16a :: Text -> Either (ParseErrorBundle Text Void) Int
day16a input = do
    gr <- Map.fromList <$> parse parser "day16.txt" input
    let next (_, _, _, _, 0) = []
        next (room, open, flow, total, time) =
          [ ( total + flow + (flow + rate) * (time - 1)
            , (room, Set.insert room open, flow + rate, total + flow, time - 1)
            )
          | rate > 0
          , Set.notMember room open
          ] ++
          [ ( total + flow * time
            , (room', open, flow, total + flow, time - 1)
            )
          | room' <- rooms
          ] where (rate, rooms) = gr Map.! room
        max' total (room, open, flow, total', time)
          | total' > total = traceShow (room, open, total', time) total'
          | otherwise = total
    pure $ foldl' max' 0 $ search next (0, ("AA", Set.empty, 0, 0, 30))
