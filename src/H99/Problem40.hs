module H99.Problem40 (solution40) where

import H99.Problem39 (solution39)

-- | (**) Goldbach's conjecture.
--
-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.
--
-- Example:
--
-- >>> solution40 28
-- (5,23)
solution40 :: Int -> (Int, Int)
solution40 n
  | odd n = error "must be even"
  | otherwise =
    let
      ps = solution39 2 (n - 2)
    in
      case [ (p1, p2) | p1 <- ps, p2 <- ps, p1 <= p2, p1 + p2 == n ] of
        [] -> error "counterexample to Goldbach's conjecture"
        pair : _ -> pair
