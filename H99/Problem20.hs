module H99.Problem20 (solution20) where

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
