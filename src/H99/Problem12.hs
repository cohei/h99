module H99.Problem12 (solution12) where

import H99.Encoded (Encoded(Single, Multiple))

-- | Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
--
-- >>> solution12 [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
-- "aaaabccaadeeee"
solution12 :: [Encoded a] -> [a]
solution12 = concatMap fromEncoded
  where
    fromEncoded :: Encoded a -> [a]
    fromEncoded (Single x)     = [x]
    fromEncoded (Multiple n x) = replicate n x
