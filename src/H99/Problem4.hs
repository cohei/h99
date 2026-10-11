module H99.Problem4 (solution4) where

-- | Find the number of elements of a list.
--
-- >>> solution4 [123, 456, 789]
-- 3
-- >>> solution4 "Hello, world!"
-- 13
solution4 :: [a] -> Int
solution4 = length
