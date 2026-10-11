module H99.Problem33 (solution33) where

import H99.Problem32 (solution32)

-- | Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
--
-- Example:
--
-- >>> solution33 35 64
-- True
solution33 :: Int -> Int -> Bool
solution33 n m = solution32 n m == 1
