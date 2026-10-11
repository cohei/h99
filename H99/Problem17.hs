module H99.Problem17 (solution17) where

-- | Split a list into two parts; the length of the first part is given.
--
-- Do not use any predefined predicates.
--
-- >>> solution17 3 "abcdefghik"
-- ("abc","defghik")
solution17 :: Int -> [a] -> ([a], [a])
solution17 = splitAt
