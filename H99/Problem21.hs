module H99.Problem21 (solution21) where

-- | Insert an element at a given position into a list.
--
-- >>> solution21 2 'X' "abcd"
-- "aXbcd"
solution21 :: Int -> a -> [a] -> [a]
solution21 _ _ []     = []
solution21 1 y xs     = y : xs
solution21 n y (x:xs) = x : solution21 (n - 1) y xs
