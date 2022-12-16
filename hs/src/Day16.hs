{-|
Module:         Day16
Description:    <https://adventofcode.com/2022/day/16 Day 16: Proboscidea Volcanium>
-}
{-# LANGUAGE OverloadedStrings, TypeFamilies, ViewPatterns #-}
module Day16 (day16a) where

import Data.Char (isAlphaNum)
import qualified Data.Heap as Heap (FstMaxPolicy, insert, singleton, view)
import Data.List (foldl')
import qualified Data.Map as Map ((!), elems, fromList)
import Data.Maybe (fromMaybe)
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

search :: (Ord a, Ord b) => (a -> (Maybe b, [(b, a)])) -> (b, a) -> [a]
search next = search' Set.empty Nothing . Heap.singleton @Heap.FstMaxPolicy where
    search' seen bestEstimate (Heap.view -> Just ((b, a), heap))
      | Set.member a seen = search' seen bestEstimate heap
      | fromMaybe False $ (<) <$> potential <*> bestEstimate = search' seen bestEstimate heap
      | otherwise = a : search' seen' (Just $ maybe b (max b) bestEstimate) heap' where
            (potential, nexts) = next a
            seen' = Set.insert a seen
            heap' = foldl' (flip Heap.insert) heap $ filter (flip Set.notMember seen' . snd) nexts
    search' _ _ _ = []

day16a :: Text -> Either (ParseErrorBundle Text Void) Int
day16a input = do
    gr <- Map.fromList <$> parse parser "day16.txt" input
    let maxFlow = sum $ fst <$> Map.elems gr
        next (_, _, _, _, 0) = (Nothing, [])
        next (room, open, flow, total, time)
          | flow == maxFlow = (Just ideal, [(ideal, (room, open, flow, ideal, 0))])
          | otherwise = (Just ideal, ) $
              [ (estimate + rate * (time - 1), (room, Set.insert room open, flow + rate, total + flow, time - 1))
              | rate > 0 && Set.notMember room open
              ] ++ [(estimate, (room', open, flow, total + flow, time - 1)) | room' <- rooms]
          where
            estimate = total + flow * time
            ideal = total + maxFlow * time
            (rate, rooms) = gr Map.! room
        max' total (room, open, flow, total', time)
          | total' > total = traceShow (room, open, total', time) total'
          | otherwise = total
    pure $ foldl' max' 0 $ search next (0, ("AA", Set.empty, 0, 0, 30))
