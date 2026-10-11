module H99.Problem14 (solution14) where

-- | Duplicate the elements of a list.
--
-- >>> solution14 [1, 2, 3]
-- [1,1,2,2,3,3]
solution14 :: [a] -> [a]
solution14 = concatMap $ replicate 2
