module H99.Problem2 (solution2) where

-- | Find the last but one element of a list.
--
-- >>> solution2 [1,2,3,4]
-- 3
-- >>> solution2 ['a'..'z']
-- 'y'
solution2 :: [a] -> a
solution2 = last . init
