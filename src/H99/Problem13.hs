module H99.Problem13 (solution13) where

import Data.List (group)
import GHC.Exts (the)

import H99.Encoded (Encoded(Single, Multiple))

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
