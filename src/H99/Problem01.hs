module H99.Problem01 (solution01) where

-- | Find the last element of a list.
--
-- >>> solution01 [1,2,3,4]
-- 4
-- >>> solution01 ['x','y','z']
-- 'z'
solution01 :: [a] -> a
solution01 = last
