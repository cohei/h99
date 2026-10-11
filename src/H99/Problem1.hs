module H99.Problem1 (solution1) where

-- | Find the last element of a list.
--
-- >>> solution1 [1,2,3,4]
-- 4
-- >>> solution1 ['x','y','z']
-- 'z'
solution1 :: [a] -> a
solution1 = last
