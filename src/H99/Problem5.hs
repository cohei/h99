module H99.Problem5 (solution5) where

-- | Reverse a list.
--
-- >>> solution5 "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- >>> solution5 [1,2,3,4]
-- [4,3,2,1]
solution5 :: [a] -> [a]
solution5 = reverse
