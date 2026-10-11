module H99.Problem08 (solution08) where

import Data.List (group)
import GHC.Exts (the)

-- | Eliminate consecutive duplicates of list elements.
--
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
--
-- >>> solution08 "aaaabccaadeeee"
-- "abcade"
solution08 :: Eq a => [a] -> [a]
solution08 = map the . group
