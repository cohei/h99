module H99.Problem26 (solution26) where

-- | Generate the combinations of K distinct objects chosen from the N elements of a list.
--
-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
--
-- >>> solution26 3 "abcdef"
-- ["abc","abd","abe",...]
solution26 :: Int -> [a] -> [[a]]
solution26 _ [] = []
solution26 0 _ = [[]]
solution26 1 xs = map (:[]) xs
solution26 n (x:xs) = map (x:) (solution26 (n - 1) xs) ++ solution26 n xs
