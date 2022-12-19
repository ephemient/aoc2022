{-|
Module:         Day19
Description:    <https://adventofcode.com/2022/day/19 Day 19: Not Enough Minerals>
-}
{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Day19 (day19a) where

import Control.Arrow (second)
import Control.Parallel.Strategies (parMap, rseq)
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

geodes :: (IsString k, Ord k, Num v, Ord v) => Int -> Map k (Map k v) -> v
geodes n blueprint = go 0 (initialRobots, initialResources, n, []) where
    initialRobots = Map.insert "ore" 1 $ const 0 <$> blueprint
    initialResources = const 0 <$> blueprint
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
    go k (_, _, 0, log) = k
    go k (robots, resources, m, log)
      | k > potential robots resources m = k
      | otherwise = foldl' go
          (max k $ resources Map.! "geode" + fromIntegral m * robots Map.! "geode")
          [ (robots', resources'', m - d - 1, (n - m + d, robot):log)
          | (robot, costs) <- Map.toList blueprint
          , (d, resources') <- take 1 $ dropWhile (any (< 0) . snd) $
                zip [0..m - 1] $ iterate (Map.unionWith (+) robots) $
                Map.unionWith (+) resources $ negate <$> costs
          , let robots' = Map.insertWith (+) robot 1 robots
                resources'' = Map.unionWith (+) robots resources'
          ]

day19a :: Text -> Either (ParseErrorBundle Text Void) Int
day19a input = do
    blueprints <- parse (parser @Int @Int @Void <* eof) "day19.txt" input
    pure . sum . fmap (uncurry (*)) .
        parMap rseq (traceShowId . second (geodes 24)) $ Map.toList blueprints
