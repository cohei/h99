module H99.Problem31 (solution31) where

-- | Determine whether a given integer number is prime.
--
-- Example:
--
-- >>> solution31 7
-- True
solution31 :: Int -> Bool
solution31 n = not $ any (\i -> n `mod` i == 0) $ takeWhile (\i -> i * i <= n) [2..]
