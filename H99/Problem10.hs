module H99.Problem10 (solution10) where

import Control.Arrow ((&&&))
import GHC.Exts (the)

import H99.Problem9 (solution9)

-- | Run-length encoding of a list.
--
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
--
-- >>> solution10 "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
solution10 :: Eq a => [a] -> [(Int, a)]
solution10 = map (length &&& the) . solution9
