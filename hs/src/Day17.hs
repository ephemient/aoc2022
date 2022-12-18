{-|
Module:         Day17
Description:    <https://adventofcode.com/2022/day/17 Day 17: Pyroclastic Flow>
-}
module Day17 (day17) where

import Control.Monad (filterM, forM_)
import Data.Bits ((.&.), (.|.), bit, setBit, shiftL, testBit)
import Data.Ix (inRange)
import Data.List (scanl')
import qualified Data.Map as Map (empty, insert, lookup)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (index, length, lines)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V (all, create, drop, fromList, iforM_, length, singleton, thaw, zipWith)
import qualified Data.Vector.Unboxed.Mutable as MV (drop, grow, iforM_, length, modify, read, replicate, write)
import Data.Word (Word8)

rocks :: [(Int, Vector Word8)]
rocks =
  [ (4, V.singleton 15)
  , (3, V.fromList [2, 7, 2])
  , (3, V.fromList [7, 4, 4])
  , (1, V.fromList [1, 1, 1, 1])
  , (2, V.fromList [3, 3])
  ]

fall :: Text -> (Int, Int, Vector Word8) -> (Int, Vector Word8) -> (Int, Int, Vector Word8)
fall jet (i, height, lines) (width, rock) = fall' i 2 (V.length lines + 3) where
    ok x y = V.all (== 0) . V.zipWith ((.&.) . (`shiftL` x)) rock $ V.drop y lines
    fall' i x y
      | not $ ok x y
      = (i, height + max 0 (y + 1 + V.length rock - V.length lines), place x $ y + 1)
      | inRange (0, 7 - width) x' && ok x' y = fall' i' x' $ y - 1
      | otherwise = fall' i' x $ y - 1
      where
        c = T.index jet i
        i' = (i + 1) `mod` T.length jet
        x' | '<' <- c = x - 1 | '>' <- c = x + 1
    place x y = V.create $ do
        lines <- V.thaw lines >>= flip MV.grow (max 0 $ y + V.length rock - V.length lines)
        V.iforM_ rock $ \dy row -> MV.modify lines (.|. row `shiftL` x) $ y + dy
        visible <- MV.replicate (MV.length lines + 1) 0
        MV.write visible (MV.length lines) $ bit 0
        let dfs [] = MV.iforM_ lines $ \y row -> MV.read visible y >>= MV.write lines y . (.&. row)
            dfs ((x, y):q) = do
                let next = [(x - 1, y) | x > 0] ++ [(x + 1, y) | x < 6] ++
                        [(x, y - 1) | y > 0] ++ [(x, y + 1) | y < MV.length lines]
                next <- flip filterM next $ \(x, y) ->
                    not . flip testBit x <$> MV.read visible y
                forM_ next $ \(x, y) -> MV.modify visible (flip setBit x) y
                next <- flip filterM next $ \(x, y) -> if y < MV.length lines
                  then not . flip testBit x <$> MV.read lines y
                  else pure True
                dfs $ next ++ q
            f y k = MV.read lines y >>= \row -> if row == 0 then k else pure $ MV.drop y lines
        dfs [(0, MV.length lines)]
        foldr f (fail "error") [0..MV.length lines - 1]

findCycle :: (Ord a) => [(a, b)] -> Maybe ((Int, b), (Int, b))
findCycle xs = listToMaybe
    [(a, b) | (b, Just a) <- zip (zip [0..] $ snd <$> xs) $ zipWith (Map.lookup . fst) xs ixs]
  where
    ixs = scanl' f Map.empty $ zip [0..] xs
    f ixs' (i, (a, b)) = Map.insert a (i, b) ixs'

day17 :: Int -> Text -> Int
day17 n input
  | Just ((i, height1), (j, height2)) <- findCycle $ take n heights
  = let (q, r) = (n - i) `divMod` (j - i) in snd (heights !! (i + r)) + q * (height2 - height1)
  | otherwise = snd $ heights !! n
  where
    [jet] = T.lines input
    heights =
      [ ((i, j, lines), height)
      | (i, (j, height, lines)) <- zip (cycle [1..length rocks]) $
            scanl' (fall jet) (0, 0, V.singleton 127) $ cycle rocks
      ]
