module H99.Problem6 (solution6) where

-- | Find out whether a list is a palindrome.
--
-- A palindrome can be read forward or backward; e.g. (x a m a x).
--
-- >>> solution6 [1,2,3]
-- False
-- >>> solution6 "madamimadam"
-- True
-- >>> solution6 [1,2,4,8,16,8,4,2,1]
-- True
solution6 :: Eq a => [a] -> Bool
solution6 []  = True
solution6 [_] = True
solution6 (x : xs)  = x == last xs && solution6 (init xs)
