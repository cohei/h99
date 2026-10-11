module H99.Problem02 (solution02) where

-- | Find the last but one element of a list.
--
-- >>> solution02 [1,2,3,4]
-- 3
-- >>> solution02 ['a'..'z']
-- 'y'
solution02 :: [a] -> a
solution02 = last . init
