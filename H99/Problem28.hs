module H99.Problem28 (solution28_1, solution28_2) where

import Data.List (sortOn)

-- | Sorting a list of lists according to length of sublists.
--
-- a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.
--
-- Example:
--
-- >>> solution28_1 ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl"]
solution28_1 :: [[a]] -> [[a]]
solution28_1 = sortOn length

-- | b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
--
-- Example:
--
-- >>> solution28_2 ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
-- ["ijkl","o","abc","fgh","de","de","mn"]
solution28_2 :: forall a. [[a]] -> [[a]]
solution28_2 xss = sortOn (length . sameLengthWith) xss
  where
    sameLengthWith :: [a] -> [[a]]
    sameLengthWith xs = filter ((length xs ==) . length) xss
