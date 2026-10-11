module H99.Problem39 (solution39) where

import H99.Problem31 (solution31)

-- | A list of prime numbers.
--
-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
--
-- Example:
--
-- >>> solution39 10 20
-- [11,13,17,19]
solution39 :: Int -> Int -> [Int]
-- solution39 = _
solution39 n m = filter solution31 [n..m]
