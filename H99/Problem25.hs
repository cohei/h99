module H99.Problem25 (solution25) where

import H99.Problem24 (solution24)

-- | Generate a random permutation of the elements of a list.
--
-- >>> import Data.List (sort)
-- >>> p <- solution25 "abcdef"
-- >>> sort p == sort "abcdef"
-- True
solution25 :: [a] -> IO [a]
solution25 xs = do
  ns <- solution24 n n
  return $ zipWith ($) (map (flip (!!) . (subtract 1)) ns) $ repeat xs
  where
    n = length xs
