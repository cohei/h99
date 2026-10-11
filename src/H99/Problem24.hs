module H99.Problem24 (solution24) where

import H99.Problem22 (solution22)
import H99.Sampling (discreteUniform)

-- | Lotto: Draw N different random numbers from the set 1..M.
--
-- >>> is <- solution24 6 49
-- >>> import Data.List (nub)
-- >>> length $ nub is
-- 6
-- >>> all (<= 49) is
-- True
solution24 :: Int -> Int -> IO [Int]
solution24 n m = go []
  where
    dist :: IO Int
    dist = discreteUniform $ solution22 1 m

    go acc
      | length acc == n = return acc
      | otherwise       = do
          i <- dist
          if i `elem` acc
            then go acc
            else go (i : acc)
