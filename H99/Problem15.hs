module H99.Problem15 (solution15) where

-- | Replicate the elements of a list a given number of times.
--
-- >>> solution15 3 "abc"
-- "aaabbbccc"
solution15 :: Int -> [a] -> [a]
solution15 = concatMap . replicate
