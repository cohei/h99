module H99.Problem09 (solution09) where

import Data.List (group)

-- | Pack consecutive duplicates of list elements into sublists.
--
-- If a list contains repeated elements they should be placed in separate sublists.
--
-- >>> solution09 "aaaabccaadeeee"
-- ["aaaa","b","cc","aa","d","eeee"]
solution09 :: Eq a => [a] -> [[a]]
solution09 = group
