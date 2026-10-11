module H99.Problem7 (solution7, NestedList(..)) where

import Data.Foldable (Foldable(toList))
import Data.Kind (Type)

-- | Flatten a nested list structure.
--
-- Transform a list, possibly holding lists as elements into a \`flat\' list by replacing each list with its elements (recursively).
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
solution7 = toList

-- | We have to define a new data type, because lists in Haskell are homogeneous.
type NestedList :: Type -> Type
data NestedList a = Elem a | List [NestedList a]
  deriving stock (Foldable)
