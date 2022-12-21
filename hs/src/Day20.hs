{-|
Module:         Day20
Description:    <https://adventofcode.com/2022/day/20 Day 20: Grove Positioning Service>
-}
{-# LANGUAGE LambdaCase, NondecreasingIndentation #-}
module Day20 (day20a, day20b) where

import Common (readEntire)
import Control.Monad (forM_, replicateM_)
import Control.Monad.ST (runST)
import Data.Bool (bool)
import Data.Text (Text)
import qualified Data.Text as T (lines)
import qualified Data.Text.Read as T (decimal, signed)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V (enumFromN, fromList, length, map, thaw)
import Data.Vector.Generic.Mutable (MVector, PrimMonad, PrimState)
import qualified Data.Vector.Generic.Mutable as MV (length, move, read, slice, write)
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as UV (zip)

parse :: (Integral a, Unbox a, Vector v a) => Text -> Either String (v a)
parse input = V.fromList <$> mapM (readEntire $ T.signed T.decimal) (T.lines input)

mvFindIndex :: (PrimMonad m, MVector v a) => (a -> Bool) -> v (PrimState m) a -> m (Maybe Int)
mvFindIndex p v = foldr f (pure Nothing) [0..MV.length v - 1] where
    f i k = MV.read v i >>= bool k (pure $ Just i) . p

mix :: (PrimMonad m, MVector v (Int, Int)) => v (PrimState m) (Int, Int) -> m ()
mix v = forM_ [0..MV.length v - 1] $ \i -> mvFindIndex ((== i) . fst) v >>= \case
    Just i -> do
        a@(_, x) <- MV.read v i
        let j = (i + x) `mod` (MV.length v - 1)
        case compare i j of
            LT -> MV.move (MV.slice i (j - i) v) (MV.slice (i + 1) (j - i) v)
            GT -> MV.move (MV.slice (j + 1) (i - j) v) (MV.slice j (i - j) v)
            _ -> pure ()
        MV.write v j a

day20a :: Text -> Either String Int
day20a input = do
    nums <- parse input
    pure $ runST $ do
    v <- V.thaw $ UV.zip (V.enumFromN 0 $ V.length nums) nums
    mix v
    Just i <- mvFindIndex ((== 0) . snd) v
    sum <$> sequence [snd <$> MV.read v ((i + x) `mod` MV.length v) | x <- [1000, 2000, 3000]]

day20b :: Text -> Either String Int
day20b input = do
    nums <- V.map (* 811589153) <$> parse input
    pure $ runST $ do
    v <- V.thaw $ UV.zip (V.enumFromN 0 $ V.length nums) nums
    replicateM_ 10 $ mix v
    Just i <- mvFindIndex ((== 0) . snd) v
    sum <$> sequence [snd <$> MV.read v ((i + x) `mod` MV.length v) | x <- [1000, 2000, 3000]]
