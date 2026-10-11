module H99.Problem22 (solution22) where

-- | Create a list containing all integers within a given range.
--
-- >>> solution22 4 9
-- [4,5,6,7,8,9]
solution22 :: Enum a => a -> a -> [a]
solution22 = enumFromTo
