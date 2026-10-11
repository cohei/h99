module H99.Problem3 (solution3) where

-- | Find the K'th element of a list.
--
-- The first element in the list is number 1.
--
-- >>> solution3 3 "abcde"
-- 'c'
-- >>> solution3 2 [1,2,3]
-- 2
-- >>> solution3 5 "haskell"
-- 'e'
solution3 :: Int -> [a] -> a
solution3 = flip (!!) . subtract 1
