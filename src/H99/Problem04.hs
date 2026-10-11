module H99.Problem04 (solution04) where

-- | Find the number of elements of a list.
--
-- >>> solution04 [123, 456, 789]
-- 3
-- >>> solution04 "Hello, world!"
-- 13
solution04 :: [a] -> Int
solution04 = length
