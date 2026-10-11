module H99.Problem47 (solution47) where

import H99.Problem46 (solution46)

-- | (*) Truth tables for logical expressions (2).
--
-- Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.
--
-- Example:
--
-- >>> infixr 3 `and'` ; infixr 2 `or'` ; and', or' :: Bool -> Bool -> Bool ; and' = (&&) ; or' = (||)
-- >>> solution47 (\a b -> a `and'` (a `or'` not b))
-- True True True
-- True False True
-- False True False
-- False False False
solution47 :: (Bool -> Bool -> Bool) -> IO ()
solution47 = solution46
