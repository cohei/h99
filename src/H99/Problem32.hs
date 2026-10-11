module H99.Problem32 (solution32) where

-- | Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
--
-- Example:
--
-- >>> [solution32 36 63, solution32 (-3) (-6), solution32 (-3) 6]
-- [9,3,3]
solution32 :: Int -> Int -> Int
solution32 n 0 = abs n
solution32 n m = solution32 m (n `mod` m)
