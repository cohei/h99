module H99.Problem27 (solution27_1, solution27_2) where

import Control.Arrow (first, second)

-- | Group the elements of a set into disjoint subsets.
--
-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
--
-- Example:
--
-- >>> solution27_1 ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [(["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]),...
-- >>> length $ solution27_1 ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- 1260
solution27_1 :: [a] -> [([a], [a], [a])]
solution27_1 xs = [ (two, three, four) | (two, seven) <- pick 2 xs, (three, four) <- pick 3 seven ]

pick :: Int -> [a] -> [([a], [a])]
pick _ [] = []
pick 0 xs = [([], xs)]
pick 1 (x:xs) = ([x], xs) : map (second (x:)) (pick 1 xs)
pick n (x:xs) = map (first (x:)) (pick (n - 1) xs) ++ map (second (x:)) (pick n xs)

-- | b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
--
-- Example:
--
-- >>> solution27_2 [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...
-- >>> length $ solution27_2 [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- 1260
--
-- >>> solution27_2 [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...
-- >>> length $ solution27_2 [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- 756
--
-- Note that we do not want permutations of the group members; i.e. @[["aldo","beat"],...]@ is the same solution as @[["beat","aldo"],...]@. However, we make a difference between @[["aldo","beat"],["carla","david"],...]@ and @[["carla","david"],["aldo","beat"],...]@.
--
-- You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
solution27_2 :: Eq a => [Int] -> [a] -> [[[a]]]
solution27_2 _ [] = [[]]
solution27_2 [] _ = [[]]
solution27_2 (n:ns) xs = [ ys : ts | (ys, zs) <- pick n xs, ts <- solution27_2 ns zs ]
