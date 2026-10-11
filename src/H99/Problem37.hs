module H99.Problem37 (solution37) where

import H99.Problem36 (solution36)

-- | Calculate Euler's totient function phi(m) (improved).
--
-- See problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let @((p1 m1) (p2 m2) (p3 m3) ...)@ be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
--
-- @
-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
--          (p2 - 1) * p2 ** (m2 - 1) *
--          (p3 - 1) * p3 ** (m3 - 1) * ...
-- @
--
-- Note that a ** b stands for the b'th power of a.
--
-- Example:
--
-- >>> solution37 10
-- 4
solution37 :: Int -> Int
solution37 = product . map (\(p, m) -> (p - 1) * p ^ (m - 1)) . solution36
