module H99.Problem16 (solution16) where

import Data.List (unfoldr)

-- | Drop every N'th element from a list.
--
-- >>> solution16 3 "abcdefghik"
-- "abdeghk"
solution16 :: Int -> [a] -> [a]
solution16 n = concat . unfoldr step
  where
    step :: [a] -> Maybe ([a], [a])
    step xs = if null xs then Nothing else Just (take (n - 1) xs, drop n xs)
