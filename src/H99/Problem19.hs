module H99.Problem19 (solution19) where

import Control.Monad (join)
import Data.Tuple (swap)

-- | Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
--
-- >>> solution19 3 ['a','b','c','d','e','f','g','h']
-- "defghabc"
-- >>> solution19 (-2) ['a','b','c','d','e','f','g','h']
-- "ghabcdef"
solution19 :: Int -> [a] -> [a]
solution19 n = join $ (uncurry (++) .) . (swap .) . splitAt . mod n . length
