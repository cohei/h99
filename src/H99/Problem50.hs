module H99.Problem50 (solution50) where

import Control.Arrow (second)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Monoid (Sum(Sum, getSum))
import Numeric.Natural (Natural)

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
