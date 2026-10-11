module H99.Problem34 (solution34) where

import H99.Problem33 (solution33)

-- | Calculate Euler's totient function phi(m).
--
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
--
-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
--
-- Example:
--
-- >>> solution34 10
-- 4
solution34 :: Int -> Int
solution34 1 = 1
solution34 n = length $ filter (solution33 n) [1..n]
