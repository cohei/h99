module H99.Problem23 (solution23) where

import Control.Monad (replicateM)

import H99.Sampling (discreteUniform)

-- | Extract a given number of randomly selected elements from a list.
--
-- >>> cs <- solution23 3 "abcdefgh"
-- >>> length cs
-- 3
-- >>> all (\c -> c `elem` "abcdefgh") cs
-- True
solution23 :: Int -> [a] -> IO [a]
solution23 n xs = replicateM n $ discreteUniform xs
