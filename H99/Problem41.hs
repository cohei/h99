module H99.Problem41 (solution41, solution41') where

import H99.Problem40 (solution40)

-- | Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
--
-- In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
--
-- Example:
--
-- >>> solution41 9 20
-- [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
-- >>> solution41' 4 2000 50
-- [(73,919),(61,1321),(67,1789),(61,1867)]
solution41 :: Int -> Int -> [(Int, Int)]
solution41 n m = map solution40 $ filter even [n..m]

solution41' :: Int -> Int -> Int -> [(Int, Int)]
solution41' n m level = filter (\(p, q) -> p > level && q > level) $ solution41 n m
