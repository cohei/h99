module H99.Problem8 (solution8) where

import Data.List (group)
import GHC.Exts (the)

-- | Eliminate consecutive duplicates of list elements.
--
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
--
-- >>> solution8 "aaaabccaadeeee"
-- "abcade"
solution8 :: Eq a => [a] -> [a]
solution8 = map the . group
