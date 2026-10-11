module H99.Problem36 (solution36) where

import Data.Tuple (swap)

import H99.Problem10 (solution10)
import H99.Problem35 (solution35)

-- | Determine the prime factors of a given positive integer.
--
-- Construct a list containing the prime factors and their multiplicity.
--
-- Example:
--
-- >>> solution36 315
-- [(3,2),(5,1),(7,1)]
solution36 :: Int -> [(Int, Int)]
solution36 = map swap . solution10 . solution35
