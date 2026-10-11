module H99.Problem46 (solution46) where

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
