{-# LANGUAGE TemplateHaskellQuotes #-}
module Day9.TH (spliceCountUnique) where

import Data.Bits ((.&.), (.|.), shiftL)
import Data.Int (Int32, Int64)
import qualified Data.IntSet as IntSet (fromList, size)
import qualified Data.Set as Set (fromList, size)
import Data.Primitive.MachDeps (sIZEOF_INT, sIZEOF_INT32)
import Language.Haskell.TH (Exp, Quote)

countUnique64 :: [(Int32, Int32)] -> Int
countUnique64 = IntSet.size . IntSet.fromList . map mix where
    mix (x, y) = fromIntegral @Int64 $ fromIntegral x `shiftL` 32 .|. fromIntegral y .&. 0xFFFFFFFF

countUnique :: [(Int, Int)] -> Int
countUnique = Set.size . Set.fromList

spliceCountUnique :: (Quote m) => m Exp
spliceCountUnique
  | sIZEOF_INT >= 2 * sIZEOF_INT32 = [| countUnique64 |]
  | otherwise = [| countUnique |]
