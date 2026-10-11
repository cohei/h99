module H99.Problem11 (solution11) where

import H99.Encoded (Encoded(Single, Multiple))
import H99.Problem10 (solution10)

-- | Modified run-length encoding.
--
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
--
-- >>> solution11 "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
solution11 :: Eq a => [a] -> [Encoded a]
solution11 = map toEncoded . solution10
  where
    toEncoded :: (Int, a) -> Encoded a
    toEncoded (1, x) = Single x
    toEncoded (n, x) = Multiple n x
