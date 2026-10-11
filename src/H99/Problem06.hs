module H99.Problem06 (solution06) where

-- | Find out whether a list is a palindrome.
--
-- A palindrome can be read forward or backward; e.g. (x a m a x).
--
-- >>> solution06 [1,2,3]
-- False
-- >>> solution06 "madamimadam"
-- True
-- >>> solution06 [1,2,4,8,16,8,4,2,1]
-- True
solution06 :: Eq a => [a] -> Bool
solution06 []  = True
solution06 [_] = True
solution06 (x : xs)  = x == last xs && solution06 (init xs)
