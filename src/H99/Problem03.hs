module H99.Problem03 (solution03) where

-- | Find the K'th element of a list.
--
-- The first element in the list is number 1.
--
-- >>> solution03 3 "abcde"
-- 'c'
-- >>> solution03 2 [1,2,3]
-- 2
-- >>> solution03 5 "haskell"
-- 'e'
solution03 :: Int -> [a] -> a
solution03 = flip (!!) . subtract 1
