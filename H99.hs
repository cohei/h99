{-|
Answers for <https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems H-99: Ninety-Nine Haskell Problems>. @doctest@ed.
-}

module H99
  ( h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , h7
  , NestedList
  , h8
  , h9
  , h10
  , h11
  , Encoded
  , h12
  , h13
  , h14
  , h15
  , h16
  , h17
  , h18
  , h19
  , h20
  , h21
  , h22
  , h23
  , h24
  , h25
  , h26
  , h27_1
  , h27_2
  , h28_1
  , h28_2
  , h31
  , h32
  , h33
  , h34
  , h35
  , h36
  , h37
  , h38
  , h39
  , h40
  , h41
  , h41'
  ) where

import Control.Arrow ((&&&), first, second)
import Control.Monad (join)
import Data.List (group, unfoldr, sortOn)
import Data.Maybe (listToMaybe)
import Data.Tuple (swap)
import GHC.Exts (the)
import System.Random.MWC.Probability (create, discreteUniform, samples, sample)

-- | Find the last element of a list.
--
-- >>> h1 [1,2,3,4]
-- 4
-- >>> h1 ['x','y','z']
-- 'z'
h1 :: [a] -> a
h1 = last

-- | Find the last but one element of a list.
--
-- >>> h2 [1,2,3,4]
-- 3
-- >>> h2 ['a'..'z']
-- 'y'
h2 :: [a] -> a
h2 = last . init

-- | Find the K'th element of a list.
--
-- The first element in the list is number 1.
--
-- >>> h3 3 "abcde"
-- 'c'
-- >>> h3 2 [1,2,3]
-- 2
-- >>> h3 5 "haskell"
-- 'e'
h3 :: Int -> [a] -> a
h3 = flip (!!) . subtract 1

-- | Find the number of elements of a list.
--
-- >>> h4 [123, 456, 789]
-- 3
-- >>> h4 "Hello, world!"
-- 13
h4 :: [a] -> Int
h4 = length

-- | Reverse a list.
--
-- >>> h5 "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- >>> h5 [1,2,3,4]
-- [4,3,2,1]
h5 :: [a] -> [a]
h5 = reverse

-- | Find out whether a list is a palindrome.
--
-- A palindrome can be read forward or backward; e.g. (x a m a x).
--
-- >>> h6 [1,2,3]
-- False
-- >>> h6 "madamimadam"
-- True
-- >>> h6 [1,2,4,8,16,8,4,2,1]
-- True
h6 :: Eq a => [a] -> Bool
h6 []  = True
h6 [_] = True
h6 xs  = head xs == last xs && h6 (init (tail xs))

-- | Flatten a nested list structure.
--
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
--
-- >>> h7 $ List [Elem 'a', List [Elem 'b', List [Elem 'c', Elem 'd'], Elem 'e']]
-- "abcde"
-- >>> h7 $ Elem 5
-- [5]
-- >>> h7 $ List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
-- [1,2,3,4,5]
-- >>> h7 $ List []
-- []
h7 :: NestedList a -> [a]
h7 (Elem x) = [x]
h7 (List xs) = xs >>= h7

-- | We have to define a new data type, because lists in Haskell are homogeneous.
data NestedList a = Elem a | List [NestedList a]

-- | Eliminate consecutive duplicates of list elements.
--
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
--
-- >>> h8 "aaaabccaadeeee"
-- "abcade"
h8 :: Eq a => [a] -> [a]
h8 = map the . group

-- | Pack consecutive duplicates of list elements into sublists.
--
-- If a list contains repeated elements they should be placed in separate sublists.
--
-- >>> h9 "aaaabccaadeeee"
-- ["aaaa","b","cc","aa","d","eeee"]
h9 :: Eq a => [a] -> [[a]]
h9 = group

-- | Run-length encoding of a list.
--
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
--
-- >>> h10 "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
h10 :: Eq a => [a] -> [(Int, a)]
h10 = map (length &&& the) . h9

-- | Run-length encoding, dealing one element as special case.
data Encoded a = Single a | Multiple Int a
  deriving Show

-- | Modified run-length encoding.
--
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
--
-- >>> h11 "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
h11 :: Eq a => [a] -> [Encoded a]
h11 = map toEncoded . h10
  where
    toEncoded (1, x) = Single x
    toEncoded (n, x) = Multiple n x

-- | Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
--
-- >>> h12 [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
-- "aaaabccaadeeee"
h12 :: [Encoded a] -> [a]
h12 = concatMap fromEncoded
  where
    fromEncoded (Single x)     = [x]
    fromEncoded (Multiple n x) = replicate n x

-- | Run-length encoding of a list (direct solution).
--
-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
--
-- >>> h13 "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
h13 :: Eq a => [a] -> [Encoded a]
h13 = map toEncoded . group
  where
    toEncoded []  = undefined
    toEncoded [x] = Single x
    toEncoded xs  = Multiple (length xs) (head xs)

-- | Duplicate the elements of a list.
--
-- >>> h14 [1, 2, 3]
-- [1,1,2,2,3,3]
h14 :: [a] -> [a]
h14 = concatMap $ replicate 2

-- | Replicate the elements of a list a given number of times.
--
-- >>> h15 3 "abc"
-- "aaabbbccc"
h15 :: Int -> [a] -> [a]
h15 = concatMap . replicate

-- | Drop every N'th element from a list.
--
-- >>> h16 3 "abcdefghik"
-- "abdeghk"
h16 :: Int -> [a] -> [a]
h16 n = concat . unfoldr step
  where
    step xs = if null xs then Nothing else Just (take (n - 1) xs, drop n xs)

-- | Split a list into two parts; the length of the first part is given.
--
-- Do not use any predefined predicates.
--
-- >>> h17 3 "abcdefghik"
-- ("abc","defghik")
h17 :: Int -> [a] -> ([a], [a])
h17 = splitAt

-- | Extract a slice from a list.
--
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
--
-- >>> h18 3 7 ['a','b','c','d','e','f','g','h','i','k']
-- "cdefg"
h18 :: Int -> Int -> [a] -> [a]
h18 i j = drop (i - 1) . take j

-- | Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
--
-- >>> h19 3 ['a','b','c','d','e','f','g','h']
-- "defghabc"
-- >>> h19 (-2) ['a','b','c','d','e','f','g','h']
-- "ghabcdef"
h19 :: Int -> [a] -> [a]
h19 n = join $ (uncurry (++) .) . (swap .) . splitAt . mod n . length

-- | Remove the K'th element from a list.
--
-- (Note that this only returns the residue list, while the Prolog version also returns the deleted element.)
--
-- >>> h20 2 "abcd"
-- ('b',"acd")
h20 :: Int -> [a] -> (a, [a])
h20 n xs = (z , ys ++ zs)
  where
    (ys, z:zs) = splitAt (n - 1) xs

-- | Insert an element at a given position into a list.
--
-- >>> h21 2 'X' "abcd"
-- "aXbcd"
h21 :: Int -> a -> [a] -> [a]
h21 _ _ []     = []
h21 1 y xs     = y : xs
h21 n y (x:xs) = x : h21 (n - 1) y xs

-- | Create a list containing all integers within a given range.
--
-- >>> h22 4 9
-- [4,5,6,7,8,9]
h22 :: Enum a => a -> a -> [a]
h22 = enumFromTo

-- | Extract a given number of randomly selected elements from a list.
--
-- >>> cs <- h23 3 "abcdefgh"
-- >>> length cs
-- 3
-- >>> all (\c -> c `elem` "abcdefgh") cs
-- True
h23 :: Int -> [a] -> IO [a]
h23 n xs = do
  gen <- create
  samples n (discreteUniform xs) gen

-- | Lotto: Draw N different random numbers from the set 1..M.
--
-- >>> is <- h24 6 49
-- >>> import Data.List (nub)
-- >>> length $ nub is
-- 6
-- >>> all (<= 49) is
-- True
h24 :: Int -> Int -> IO [Int]
h24 n m = create >>= go []
  where
    dist = discreteUniform $ h22 1 m
    go acc gen
      | length acc == n = return acc
      | otherwise       = do
          i <- sample dist gen
          if i `elem` acc
            then go acc gen
            else go (i : acc) gen


-- | Generate a random permutation of the elements of a list.
--
-- >>> import Data.List (sort)
-- >>> p <- h25 "abcdef"
-- >>> sort p == sort "abcdef"
-- True
h25 :: [a] -> IO [a]
h25 xs = do
  ns <- h24 n n
  return $ zipWith ($) (map (flip (!!) . (subtract 1)) ns) $ repeat xs
  where
    n = length xs

-- | Generate the combinations of K distinct objects chosen from the N elements of a list.
--
-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
--
-- >>> h26 3 "abcdef"
-- ["abc","abd","abe",...]
h26 :: Int -> [a] -> [[a]]
h26 _ [] = []
h26 0 _ = [[]]
h26 1 xs = map (:[]) xs
h26 n (x:xs) = map (x:) (h26 (n - 1) xs) ++ h26 n xs

-- | Group the elements of a set into disjoint subsets.
--
-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
--
-- Example:
--
-- >>> head $ h27_1 ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- (["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"])
-- >>> length $ h27_1 ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- 1260
h27_1 :: [a] -> [([a], [a], [a])]
h27_1 xs = [ (two, three, four) | (two, seven) <- pick 2 xs, (three, four) <- pick 3 seven ]

pick :: Int -> [a] -> [([a], [a])]
pick _ [] = []
pick 0 xs = [([], xs)]
pick 1 (x:xs) = ([x], xs) : map (second (x:)) (pick 1 xs)
pick n (x:xs) = map (first (x:)) (pick (n - 1) xs) ++ map (second (x:)) (pick n xs)

-- | b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
--
-- Example:
--
-- >>> head $ h27_2 [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]]
-- >>> length $ h27_2 [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- 1260
--
-- >>> head $ h27_2 [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]]
-- >>> length $ h27_2 [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- 756
--
-- Note that we do not want permutations of the group members; i.e. @[["aldo","beat"],...]@ is the same solution as @[["beat","aldo"],...]@. However, we make a difference between @[["aldo","beat"],["carla","david"],...]@ and @[["carla","david"],["aldo","beat"],...]@.
--
-- You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
h27_2 :: Eq a => [Int] -> [a] -> [[[a]]]
h27_2 _ [] = [[]]
h27_2 [] _ = [[]]
h27_2 (n:ns) xs = [ ys : ts | (ys, zs) <- pick n xs, ts <- h27_2 ns zs ]

-- | Sorting a list of lists according to length of sublists.
--
-- a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.
--
-- Example:
--
-- >>> h28_1 ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl"]
h28_1 :: [[a]] -> [[a]]
h28_1 = sortOn length

-- | b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
--
-- Example:
--
-- >>> h28_2 ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
-- ["ijkl","o","abc","fgh","de","de","mn"]
h28_2 :: [[a]] -> [[a]]
h28_2 xss = sortOn (length . sameLengthWith) xss
  where
    sameLengthWith xs = filter ((length xs ==) . length) xss

-- | Determine whether a given integer number is prime.
--
-- Example:
--
-- >>> h31 7
-- True
h31 :: Int -> Bool
h31 n = not $ any (\i -> n `mod` i == 0) $ takeWhile (\i -> i * i <= n) [2..]

-- | Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
--
-- Example:
--
-- >>> [h32 36 63, h32 (-3) (-6), h32 (-3) 6]
-- [9,3,3]
h32 :: Int -> Int -> Int
h32 n 0 = abs n
h32 n m = h32 m (n `mod` m)

-- | Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
--
-- Example:
--
-- >>> h33 35 64
-- True
h33 :: Int -> Int -> Bool
h33 n m = h32 n m == 1

-- | Calculate Euler's totient function phi(m).
--
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
--
-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
--
-- Example:
--
-- >>> h34 10
-- 4
h34 :: Int -> Int
h34 1 = 1
h34 n = length $ filter (h33 n) [1..n]

-- | Determine the prime factors of a given positive integer.
--
-- Construct a flat list containing the prime factors in ascending order.
--
-- Example:
--
-- >>> h35 315
-- [3,3,5,7]
h35 :: Int -> [Int]
h35 n = case firstFactor n of
  Nothing -> [n]
  Just (factor, next) -> factor : h35 next

firstFactor :: Int -> Maybe (Int, Int)
firstFactor n = listToMaybe [ (m, d) | m <- candidates, let (d, r) = n `divMod` m, r == 0 ]
  where
    candidates = takeWhile (\m -> m * m <= n) [2..]

-- | Determine the prime factors of a given positive integer.
--
-- Construct a list containing the prime factors and their multiplicity.
--
-- Example:
--
-- >>> h36 315
-- [(3,2),(5,1),(7,1)]
h36 :: Int -> [(Int, Int)]
h36 = map swap . h10 . h35

-- | Calculate Euler's totient function phi(m) (improved).
--
-- See problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let @((p1 m1) (p2 m2) (p3 m3) ...)@ be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
--
-- @
-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
--          (p2 - 1) * p2 ** (m2 - 1) *
--          (p3 - 1) * p3 ** (m3 - 1) * ...
-- @
--
-- Note that a ** b stands for the b'th power of a.
--
-- Example:
--
-- >>> h37 10
-- 4
h37 :: Int -> Int
h37 = product . map (\(p, m) -> (p - 1) * p ^ (m - 1)) . h36

-- | Compare the two methods of calculating Euler's totient function.
--
-- Use the solutions of problems 34 and 37 to compare the algorithms. Take the number of reductions as a measure for efficiency. Try to calculate phi(10090) as an example.
--
-- >>> h38
-- True
h38 = h34 10090 == h37 10090

-- | A list of prime numbers.
--
-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
--
-- Example:
--
-- >>> h39 10 20
-- [11,13,17,19]
h39 :: Int -> Int -> [Int]
-- h39 = _
h39 n m = filter h31 [n..m]

-- | (**) Goldbach's conjecture.
--
-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.
--
-- Example:
--
-- >>> h40 28
-- (5,23)
h40 :: Int -> (Int, Int)
h40 n
  | odd n = error "must be even"
  | otherwise =
    let
      ps = h39 2 (n - 2)
    in
      head [ (p1, p2) | p1 <- ps, p2 <- ps, p1 <= p2, p1 + p2 == n ]

-- | Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
--
-- In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
--
-- Example:
--
-- >>> h41 9 20
-- [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
-- >>> h41' 4 2000 50
-- [(73,919),(61,1321),(67,1789),(61,1867)]
h41 :: Int -> Int -> [(Int, Int)]
h41 n m = map h40 $ filter even [n..m]

h41' :: Int -> Int -> Int -> [(Int, Int)]
h41' n m level = filter (\(p, q) -> p > level && q > level) $ h41 n m
