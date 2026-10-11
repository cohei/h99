module H99.Problem35 (solution35) where

import Data.Maybe (listToMaybe)

-- | Determine the prime factors of a given positive integer.
--
-- Construct a flat list containing the prime factors in ascending order.
--
-- Example:
--
-- >>> solution35 315
-- [3,3,5,7]
solution35 :: Int -> [Int]
solution35 n = case firstFactor n of
  Nothing -> [n]
  Just (factor, next) -> factor : solution35 next

firstFactor :: Int -> Maybe (Int, Int)
firstFactor n = listToMaybe [ (m, d) | m <- candidates, let (d, r) = n `divMod` m, r == 0 ]
  where
    candidates = takeWhile (\m -> m * m <= n) [2..]
