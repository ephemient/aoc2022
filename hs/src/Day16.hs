{-|
Module:         Day16
Description:    <https://adventofcode.com/2022/day/16 Day 16: Proboscidea Volcanium>
-}
{-# LANGUAGE OverloadedStrings, TypeFamilies, ViewPatterns #-}
module Day16 (day16a) where

import Control.Monad (join)
import Data.Char (isAlphaNum)
import qualified Data.Heap as Heap (FstMaxPolicy, insert, singleton, view)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map ((!), (!?), assocs, delete, filter, fromList, fromSet, insertWith, keys, union)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid (Sum(Sum, getSum))
import qualified Data.Set as Set (empty, foldl', fromList, insert, mapMonotonic, member, notMember)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
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

shortestPaths :: (Ord a, Monoid b, Ord b) => Map (a, a) b -> Map (a, a) b
shortestPaths es =
    Set.foldl' (\d a -> Set.foldl' (\d b -> Set.foldl' (flip $ update a b) d vs) d vs) d0 vs
  where
    vs = Set.fromList $ concat [[a, b] | (a, b) <- Map.keys es]
    d0 = Map.union es $ Map.fromSet (const mempty) $ Set.mapMonotonic (join (,)) vs
    update c b a d
      | Just x <- d Map.!? (a, c)
      , Just y <- d Map.!? (c, b)
      = Map.insertWith min (a, b) (x <> y) d
      | otherwise = d

day16a :: Text -> Either (ParseErrorBundle Text Void) Int
day16a input = do
    gr <- Map.fromList <$> parse parser "day16.txt" input
    let distances = fmap getSum . shortestPaths $ Map.fromList
            [((a, b), Sum 1) | (a, (_, bs)) <- Map.assocs gr, b <- bs]
        next (_, _, _, _, 0) = (Nothing, [])
        next (room, valves, flow, total, time)
          | null options = (Just estimate, [(estimate, (room, valves, flow, estimate, 0))])
          | otherwise = (Just potential, options)
          where
            estimate = total + flow * time
            potential = estimate + sum
              [ rate * (time - d - 1)
              | (room', rate) <- Map.assocs valves
              , d <- maybeToList $ distances Map.!? (room, room')
              , d < time
              ]
            options =
              [ ( estimate + rate * (time - d - 1)
                , (room', Map.delete room' valves, flow + rate, total + flow * (d + 1), time - d - 1)
                )
              | (room', rate) <- Map.assocs valves
              , d <- maybeToList $ distances Map.!? (room, room')
              , d < time
              ]
    pure $ maximum
        [total | (_, _, _, total, _) <- search next (0, ("AA", Map.filter (> 0) $ fst <$> gr, 0, 0, 30))]
