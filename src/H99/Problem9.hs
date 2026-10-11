module H99.Problem9 (solution9) where

import Data.List (group)

-- | Pack consecutive duplicates of list elements into sublists.
--
-- If a list contains repeated elements they should be placed in separate sublists.
--
-- >>> solution9 "aaaabccaadeeee"
-- ["aaaa","b","cc","aa","d","eeee"]
solution9 :: Eq a => [a] -> [[a]]
solution9 = group
