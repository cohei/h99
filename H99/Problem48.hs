module H99.Problem48 (solution48) where

import Control.Monad (replicateM)

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
