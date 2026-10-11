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

import Control.Arrow (second)
import Control.Monad (replicateM)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (listToMaybe)
import Data.Monoid (Sum(Sum, getSum))
import Data.Tuple (swap)
import Numeric.Natural (Natural)

import H99.Problem1 (solution1)
import H99.Problem2 (solution2)
import H99.Problem3 (solution3)
import H99.Problem4 (solution4)
import H99.Problem5 (solution5)
import H99.Problem6 (solution6)
import H99.Problem7 (solution7)
import H99.Problem8 (solution8)
import H99.Problem9 (solution9)
import H99.Problem10 (solution10)
import H99.Problem11 (solution11)
import H99.Problem12 (solution12)
import H99.Problem13 (solution13)
import H99.Problem14 (solution14)
import H99.Problem15 (solution15)
import H99.Problem16 (solution16)
import H99.Problem17 (solution17)
import H99.Problem18 (solution18)
import H99.Problem19 (solution19)
import H99.Problem20 (solution20)
import H99.Problem21 (solution21)
import H99.Problem22 (solution22)
import H99.Problem23 (solution23)
import H99.Problem24 (solution24)
import H99.Problem25 (solution25)
import H99.Problem26 (solution26)
import H99.Problem27 (solution27_1, solution27_2)
import H99.Problem28 (solution28_1, solution28_2)
import H99.Problem31 (solution31)
import H99.Problem32 (solution32)
import H99.Problem33 (solution33)
import H99.Problem34 (solution34)

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
