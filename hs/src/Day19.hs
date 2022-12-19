{-|
Module:         Day19
Description:    <https://adventofcode.com/2022/day/19 Day 19: Not Enough Minerals>
-}
{-# LANGUAGE OverloadedStrings, TypeFamilies, ViewPatterns #-}
module Day19 (day19a) where

import Data.Char (isAlphaNum)
import qualified Data.Heap as Heap (FstMaxPolicy, insert, singleton, view)
import Data.List (foldl', scanl', transpose)
import Data.Map (Map)
import qualified Data.Map as Map ((!), fromDistinctAscList, fromList, keys, insert, insertWith, toAscList, toList, unionWith)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace (traceShowId)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, between, eof, many, parse, sepBy1, sepEndBy, takeWhile1P)
import Text.Megaparsec.Char (char, space, string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal)

parser :: (Integral a, Integral b, MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Map a (Map (Tokens s) (Map (Tokens s) b)))
parser = Map.fromList <$> many blueprint where
    blueprint = (,) <$> between (string "Blueprint ") (char ':' >> space) L.decimal <*>
        (Map.fromList <$> (robot <* char '.') `sepEndBy` space)
    robot = (,) <$> between (string "Each ") (string " robot costs ") name <*>
        (Map.fromList <$> ore `sepBy1` string " and ")
    ore = flip (,) <$> L.decimal <*> (char ' ' >> name)
    name = takeWhile1P Nothing isAlphaNum

search :: (Ord a, Ord b) => (a -> (Maybe b, [(b, a)])) -> (b, a) -> [a]
search next = search' Nothing . Heap.singleton @Heap.FstMaxPolicy where
    search' bestEstimate (Heap.view -> Just ((b, a), heap))
      | fromMaybe False $ (<) <$> potential <*> bestEstimate = search' bestEstimate heap
      | otherwise = a : search' (Just $ maybe b (max b) bestEstimate) heap' where
            (potential, nexts) = next a
            heap' = foldl' (flip Heap.insert) heap nexts
    search' _ _ = []

geodes :: (IsString k, Ord k, Num v, Ord v) => Int -> Map k (Map k v) -> v
geodes n blueprint = foldl' max' 0 $ search next (0, (initialRobots, initialResources, n, [])) where
    initialRobots = Map.insert "ore" 1 $ const 0 <$> blueprint
    initialResources = const 0 <$> blueprint
    next (robots, resources, m, log) = (Just $ potential robots resources m, options) where
        estimate = resources Map.! "geode" + fromIntegral m * robots Map.! "geode"
        options =
          [ (estimate', (robots', resources'', m - d - 1, (n - m + d, robot):log))
          | (robot, costs) <- Map.toList blueprint
          , (d, resources') <- take 1 $ dropWhile (any (< 0) . snd) $
                zip [0..m - 1] $ iterate (Map.unionWith (+) robots) $
                Map.unionWith (+) resources $ negate <$> costs
          , let estimate' = if robot == "geode" then estimate + fromIntegral (d - 1) else estimate
                robots' = Map.insertWith (+) robot 1 robots
                resources'' = Map.unionWith (+) robots resources'
          ]
    potential robots resources m = potentialResources !! m Map.! "geode" where
        potentialRobots = Map.fromDistinctAscList . zip (Map.keys blueprint) <$> transpose
          [ (robots Map.! robot +) <$> scanl' f 0 potentialResources
          | (robot, costs) <- Map.toAscList blueprint
          , let f new resources'
                  | and [resources' Map.! key >= new * cost | (key, cost) <- Map.toList costs]
                  = new + 1
                  | otherwise = new
          ]
        potentialResources = scanl' (Map.unionWith (+)) resources potentialRobots
    max' prev (robots, resources, m, log) =
        max prev $ resources Map.! "geode" + fromIntegral m * robots Map.! "geode"

day19a :: Text -> Either (ParseErrorBundle Text Void) Int
day19a input = do
    blueprints <- parse (parser @Int @Int @Void <* eof) "day19.txt" input
    pure $ sum $ uncurry (*) <$> traceShowId [(i, geodes 24 blueprint) | (i, blueprint) <- Map.toList blueprints]
