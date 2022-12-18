module Common (count, crt, egcd, readEntire) where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T (null)
import Data.Text.Read (Reader)

-- |Chinese remainder theorem.
--
-- prop> crt (r1, q1) (r2, q2) == (r3, q3) ==>
--           r3 `mod` q1 == r1 && q3 `mod` q1 == 0 &&
--           r3 `mod` q2 == r2 && q3 `mod` q2 == 0
crt :: (Integral a) => (a, a) -> (a, a) -> (a, a)
crt (r1, q1) (r2, q2) = (r3 `mod` q3, q3) where
    q3 = lcm q1 q2
    -- r3 * q2 == r1 * q2 (mod q3)
    -- r3 * q3 == r2 * q1 (mod q3)
    -- r3 * (q1 + q2) = r1 * q2 + r2 * q1 (mod q3)
    (t, _, g) = egcd (q1 + q2) q3
    -- t * (q1 + q2) == g (mod q3)
    -- r3 = (r1 * q2 + r2 * q1) * t / g (mod q3)
    (r3, 0) = ((r1 * q2 + r2 * q1) * t) `divMod` g

-- |Extended GCD.
--
-- prop> gcd a b == (s, t, g) ==> a * s + b * t == g
egcd :: (Integral a) => a -> a -> (a, a, a)
egcd a 0 = (1, 0, a)
egcd a b = (t, s - q * t, g) where
    (q, r) = a `quotRem` b
    (s, t, g) = egcd b r

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count p = length . filter p . toList

readEntire :: Reader a -> Text -> Either String a
readEntire reader input = do
    (a, t) <- reader input
    if T.null t then Right a else Left "incomplete read"
