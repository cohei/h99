module H99.Problem05 (solution05) where

-- | Reverse a list.
--
-- >>> solution05 "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- >>> solution05 [1,2,3,4]
-- [4,3,2,1]
solution05 :: [a] -> [a]
solution05 = reverse
