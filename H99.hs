{-|
Answers for <https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems H-99: Ninety-Nine Haskell Problems>. @doctest@ed.
-}

module H99
  ( solution1
  , solution2
  , solution3
  , solution4
  , solution5
  , solution6
  , solution7
  , solution8
  , solution9
  , solution10
  , solution11
  , solution12
  , solution13
  , solution14
  , solution15
  , solution16
  , solution17
  , solution18
  , solution19
  , solution20
  , solution21
  , solution22
  , solution23
  , solution24
  , solution25
  , solution26
  , solution27_1
  , solution27_2
  , solution28_1
  , solution28_2
  , solution31
  , solution32
  , solution33
  , solution34
  , solution35
  , solution36
  , solution37
  , solution38
  , solution39
  , solution40
  , solution41
  , solution41'
  , solution46
  , solution47
  , solution48
  , solution49
  , solution50
  ) where

import Control.Arrow ((&&&), first, second)
import Control.Monad (join, replicateM)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List (group, unfoldr, sortOn)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (listToMaybe)
import Data.Monoid (Sum(Sum, getSum))
import Data.Tuple (swap)
import GHC.Exts (the)
import Numeric.Natural (Natural)
import System.Random (randomRIO)

-- | Find the last element of a list.
--
-- >>> solution1 [1,2,3,4]
-- 4
-- >>> solution1 ['x','y','z']
-- 'z'
solution1 :: [a] -> a
solution1 = last

-- | Find the last but one element of a list.
--
-- >>> solution2 [1,2,3,4]
-- 3
-- >>> solution2 ['a'..'z']
-- 'y'
solution2 :: [a] -> a
solution2 = last . init

-- | Find the K'th element of a list.
--
-- The first element in the list is number 1.
--
-- >>> solution3 3 "abcde"
-- 'c'
-- >>> solution3 2 [1,2,3]
-- 2
-- >>> solution3 5 "haskell"
-- 'e'
solution3 :: Int -> [a] -> a
solution3 = flip (!!) . subtract 1

-- | Find the number of elements of a list.
--
-- >>> solution4 [123, 456, 789]
-- 3
-- >>> solution4 "Hello, world!"
-- 13
solution4 :: [a] -> Int
solution4 = length

-- | Reverse a list.
--
-- >>> solution5 "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- >>> solution5 [1,2,3,4]
-- [4,3,2,1]
solution5 :: [a] -> [a]
solution5 = reverse

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

-- | Flatten a nested list structure.
--
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
--
-- >>> solution7 $ List [Elem 'a', List [Elem 'b', List [Elem 'c', Elem 'd'], Elem 'e']]
-- "abcde"
-- >>> solution7 $ Elem 5
-- [5]
-- >>> solution7 $ List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
-- [1,2,3,4,5]
-- >>> solution7 $ List []
-- []
solution7 :: NestedList a -> [a]
solution7 (Elem x) = [x]
solution7 (List xs) = xs >>= solution7

-- | We have to define a new data type, because lists in Haskell are homogeneous.
type NestedList :: Type -> Type
data NestedList a = Elem a | List [NestedList a]

-- | Eliminate consecutive duplicates of list elements.
--
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
--
-- >>> solution8 "aaaabccaadeeee"
-- "abcade"
solution8 :: Eq a => [a] -> [a]
solution8 = map the . group

-- | Pack consecutive duplicates of list elements into sublists.
--
-- If a list contains repeated elements they should be placed in separate sublists.
--
-- >>> solution9 "aaaabccaadeeee"
-- ["aaaa","b","cc","aa","d","eeee"]
solution9 :: Eq a => [a] -> [[a]]
solution9 = group

-- | Run-length encoding of a list.
--
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
--
-- >>> solution10 "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
solution10 :: Eq a => [a] -> [(Int, a)]
solution10 = map (length &&& the) . solution9

-- | Run-length encoding, dealing one element as special case.
type Encoded :: Type -> Type
data Encoded a = Single a | Multiple Int a
  deriving stock Show

-- | Modified run-length encoding.
--
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
--
-- >>> solution11 "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
solution11 :: Eq a => [a] -> [Encoded a]
solution11 = map toEncoded . solution10
  where
    toEncoded :: (Int, a) -> Encoded a
    toEncoded (1, x) = Single x
    toEncoded (n, x) = Multiple n x

-- | Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
--
-- >>> solution12 [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
-- "aaaabccaadeeee"
solution12 :: [Encoded a] -> [a]
solution12 = concatMap fromEncoded
  where
    fromEncoded :: Encoded a -> [a]
    fromEncoded (Single x)     = [x]
    fromEncoded (Multiple n x) = replicate n x

-- | Run-length encoding of a list (direct solution).
--
-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
--
-- >>> solution13 "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
solution13 :: forall a. (Eq a) => [a] -> [Encoded a]
solution13 = map toEncoded . group
  where
    toEncoded :: [a] -> Encoded a
    toEncoded []  = undefined
    toEncoded [x] = Single x
    toEncoded xs  = Multiple (length xs) (the xs)

-- | Duplicate the elements of a list.
--
-- >>> solution14 [1, 2, 3]
-- [1,1,2,2,3,3]
solution14 :: [a] -> [a]
solution14 = concatMap $ replicate 2

-- | Replicate the elements of a list a given number of times.
--
-- >>> solution15 3 "abc"
-- "aaabbbccc"
solution15 :: Int -> [a] -> [a]
solution15 = concatMap . replicate

-- | Drop every N'th element from a list.
--
-- >>> solution16 3 "abcdefghik"
-- "abdeghk"
solution16 :: Int -> [a] -> [a]
solution16 n = concat . unfoldr step
  where
    step :: [a] -> Maybe ([a], [a])
    step xs = if null xs then Nothing else Just (take (n - 1) xs, drop n xs)

-- | Split a list into two parts; the length of the first part is given.
--
-- Do not use any predefined predicates.
--
-- >>> solution17 3 "abcdefghik"
-- ("abc","defghik")
solution17 :: Int -> [a] -> ([a], [a])
solution17 = splitAt

-- | Extract a slice from a list.
--
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
--
-- >>> solution18 3 7 ['a','b','c','d','e','f','g','h','i','k']
-- "cdefg"
solution18 :: Int -> Int -> [a] -> [a]
solution18 i j = drop (i - 1) . take j

-- | Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
--
-- >>> solution19 3 ['a','b','c','d','e','f','g','h']
-- "defghabc"
-- >>> solution19 (-2) ['a','b','c','d','e','f','g','h']
-- "ghabcdef"
solution19 :: Int -> [a] -> [a]
solution19 n = join $ (uncurry (++) .) . (swap .) . splitAt . mod n . length

-- | Remove the K'th element from a list.
--
-- (Note that this only returns the residue list, while the Prolog version also returns the deleted element.)
--
-- >>> solution20 2 "abcd"
-- ('b',"acd")
solution20 :: Int -> [a] -> (a, [a])
solution20 n xs =
  case splitAt (n - 1) xs of
    (_, []) -> error "out of bound"
    (ys, z:zs) -> (z , ys ++ zs)

-- | Insert an element at a given position into a list.
--
-- >>> solution21 2 'X' "abcd"
-- "aXbcd"
solution21 :: Int -> a -> [a] -> [a]
solution21 _ _ []     = []
solution21 1 y xs     = y : xs
solution21 n y (x:xs) = x : solution21 (n - 1) y xs

-- | Create a list containing all integers within a given range.
--
-- >>> solution22 4 9
-- [4,5,6,7,8,9]
solution22 :: Enum a => a -> a -> [a]
solution22 = enumFromTo

-- | Extract a given number of randomly selected elements from a list.
--
-- >>> cs <- solution23 3 "abcdefgh"
-- >>> length cs
-- 3
-- >>> all (\c -> c `elem` "abcdefgh") cs
-- True
solution23 :: Int -> [a] -> IO [a]
solution23 n xs = replicateM n $ discreteUniform xs

discreteUniform :: [a] -> IO a
discreteUniform xs = (xs !!) <$> randomRIO (0, length xs - 1)

-- | Lotto: Draw N different random numbers from the set 1..M.
--
-- >>> is <- solution24 6 49
-- >>> import Data.List (nub)
-- >>> length $ nub is
-- 6
-- >>> all (<= 49) is
-- True
solution24 :: Int -> Int -> IO [Int]
solution24 n m = go []
  where
    dist :: IO Int
    dist = discreteUniform $ solution22 1 m

    go acc
      | length acc == n = return acc
      | otherwise       = do
          i <- dist
          if i `elem` acc
            then go acc
            else go (i : acc)

-- | Generate a random permutation of the elements of a list.
--
-- >>> import Data.List (sort)
-- >>> p <- solution25 "abcdef"
-- >>> sort p == sort "abcdef"
-- True
solution25 :: [a] -> IO [a]
solution25 xs = do
  ns <- solution24 n n
  return $ zipWith ($) (map (flip (!!) . (subtract 1)) ns) $ repeat xs
  where
    n = length xs

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

-- | Sorting a list of lists according to length of sublists.
--
-- a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.
--
-- Example:
--
-- >>> solution28_1 ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl"]
solution28_1 :: [[a]] -> [[a]]
solution28_1 = sortOn length

-- | b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
--
-- Example:
--
-- >>> solution28_2 ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
-- ["ijkl","o","abc","fgh","de","de","mn"]
solution28_2 :: forall a. [[a]] -> [[a]]
solution28_2 xss = sortOn (length . sameLengthWith) xss
  where
    sameLengthWith :: [a] -> [[a]]
    sameLengthWith xs = filter ((length xs ==) . length) xss

-- | Determine whether a given integer number is prime.
--
-- Example:
--
-- >>> solution31 7
-- True
solution31 :: Int -> Bool
solution31 n = not $ any (\i -> n `mod` i == 0) $ takeWhile (\i -> i * i <= n) [2..]

-- | Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
--
-- Example:
--
-- >>> [solution32 36 63, solution32 (-3) (-6), solution32 (-3) 6]
-- [9,3,3]
solution32 :: Int -> Int -> Int
solution32 n 0 = abs n
solution32 n m = solution32 m (n `mod` m)

-- | Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
--
-- Example:
--
-- >>> solution33 35 64
-- True
solution33 :: Int -> Int -> Bool
solution33 n m = solution32 n m == 1

-- | Calculate Euler's totient function phi(m).
--
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
--
-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
--
-- Example:
--
-- >>> solution34 10
-- 4
solution34 :: Int -> Int
solution34 1 = 1
solution34 n = length $ filter (solution33 n) [1..n]

-- | Determine the prime factors of a given positive integer.
--
-- Construct a flat list containing the prime factors in ascending order.
--
-- Example:
--
-- >>> solution35 315
-- [3,3,5,7]
solution35 :: Int -> [Int]
solution35 n = case firstFactor n of
  Nothing -> [n]
  Just (factor, next) -> factor : solution35 next

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
-- >>> solution36 315
-- [(3,2),(5,1),(7,1)]
solution36 :: Int -> [(Int, Int)]
solution36 = map swap . solution10 . solution35

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
-- >>> solution37 10
-- 4
solution37 :: Int -> Int
solution37 = product . map (\(p, m) -> (p - 1) * p ^ (m - 1)) . solution36

-- | Compare the two methods of calculating Euler's totient function.
--
-- Use the solutions of problems 34 and 37 to compare the algorithms. Take the number of reductions as a measure for efficiency. Try to calculate phi(10090) as an example.
--
-- >>> solution38
-- True
solution38 :: Bool
solution38 = solution34 10090 == solution37 10090

-- | A list of prime numbers.
--
-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
--
-- Example:
--
-- >>> solution39 10 20
-- [11,13,17,19]
solution39 :: Int -> Int -> [Int]
-- solution39 = _
solution39 n m = filter solution31 [n..m]

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

-- | (**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.
--
-- A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).
--
-- Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.
--
-- Example:
--
-- >>> infixr 3 `and'` ; infixr 2 `or'` ; and', or' :: Bool -> Bool -> Bool ; and' = (&&) ; or' = (||)
-- >>> solution46 (\a b -> (and' a (or' a b)))
-- True True True
-- True False True
-- False True False
-- False False False
solution46 :: (Bool -> Bool -> Bool) -> IO ()
solution46 f = mapM_ (\(b1, b2, b) -> putStrLn $ unwords $ map show [b1, b2, b]) $ (\b1 b2 -> (b1, b2, f b1 b2)) <$> booleans <*> booleans
  where
    booleans = [True, False]

-- | (*) Truth tables for logical expressions (2).
--
-- Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.
--
-- Example:
--
-- >>> infixr 3 `and'` ; infixr 2 `or'` ; and', or' :: Bool -> Bool -> Bool ; and' = (&&) ; or' = (||)
-- >>> solution47 (\a b -> a `and'` (a `or'` not b))
-- True True True
-- True False True
-- False True False
-- False False False
solution47 :: (Bool -> Bool -> Bool) -> IO ()
solution47 = solution46

-- | (**) Truth tables for logical expressions (3).
--
-- Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.
--
-- Example:
--
-- >>> infixr 3 `and'` ; infixr 2 `or'` ; and', or' :: Bool -> Bool -> Bool ; and' = (&&) ; or' = (||)
-- >>> infix 1 `equ'` ; equ' :: Bool -> Bool -> Bool ; equ' = (==)
-- >>> solution48 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False True
-- False True  True  True
-- False True  False True
-- False False True  True
-- False False False True
solution48 :: Int -> ([Bool] -> Bool) -> IO ()
solution48 n f = mapM_ (putStrLn . unwords . map show') $ [ bs ++ [f bs] | bs <- replicateM n booleans ]
  where
    booleans = [True, False]

    show' False = show False
    show' True  = show True ++ " "

-- | (**) Gray codes.
--
-- An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
--
-- n = 1: C(1) = ['0','1'].
-- n = 2: C(2) = ['00','01','11','10'].
-- n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
-- Find out the construction rules and write a predicate with the following specification:
--
-- % gray(N,C) :- C is the N-bit Gray code
-- Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly?
--
-- Example:
--
-- >>> solution49 3
-- ["000","001","011","010","110","111","101","100"]
solution49 :: Int -> [String]
solution49 1 = ["0", "1"]
solution49 n = map ('0' :) gray' ++ map ('1' :) (reverse gray')
  where
    gray' = solution49 (n - 1)

-- | (***) Huffman codes.
--
-- We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S. In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be performed by the predicate huffman/2 defined as follows:
--
-- % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
--
-- Example:
--
-- >>> solution50 [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
-- [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
solution50 :: [(Char, Natural)] -> [(Char, String)]
solution50 = maybe [] (map (second showBits) . sortOn fst . huffmanCode . toHuffmanTree) . nonEmpty
  where
    showBits :: [Bool] -> String
    showBits = map $ \b -> if b then '1' else '0'

type BinTree :: Type -> Type
data BinTree a =
    Leaf a
  | Node (BinTree a) (BinTree a)
  deriving stock (Show, Functor, Foldable)

type HuffmanTree :: Type
type HuffmanTree = BinTree Char

frequency :: BinTree (Char, Natural) -> Natural
frequency = getSum . foldMap (Sum . snd)

toHuffmanTree :: NonEmpty (Char, Natural) -> HuffmanTree
toHuffmanTree = fmap fst . NonEmpty.head . until isSingleton joinSmallestTwo . fmap Leaf

joinSmallestTwo :: NonEmpty (BinTree (Char, Natural)) -> NonEmpty (BinTree (Char, Natural))
joinSmallestTwo ts =
  case NonEmpty.sortWith frequency ts of
    t1 :| t2 : ts' -> Node t1 t2 :| ts'
    sorted -> sorted

isSingleton :: NonEmpty a -> Bool
isSingleton (_ :| []) = True
isSingleton _ = False

huffmanCode :: HuffmanTree -> [(Char, [Bool])]
huffmanCode = toList . label []
  where
    label :: [Bool] -> HuffmanTree -> BinTree (Char, [Bool])
    label code (Leaf c) = Leaf (c, reverse code)
    label code (Node t1 t2) = Node (label (False : code) t1) (label (True : code) t2)
