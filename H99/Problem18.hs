module H99.Problem18 (solution18) where

-- | Extract a slice from a list.
--
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
--
-- >>> solution18 3 7 ['a','b','c','d','e','f','g','h','i','k']
-- "cdefg"
solution18 :: Int -> Int -> [a] -> [a]
solution18 i j = drop (i - 1) . take j
