module H99.Encoded (Encoded(..)) where

import Data.Kind (Type)

-- | Run-length encoding, dealing one element as special case.
type Encoded :: Type -> Type
data Encoded a = Single a | Multiple Int a
  deriving stock Show
