module H99.Problem38 (solution38) where

import H99.Problem34 (solution34)
import H99.Problem37 (solution37)

-- | Compare the two methods of calculating Euler's totient function.
--
-- Use the solutions of problems 34 and 37 to compare the algorithms. Take the number of reductions as a measure for efficiency. Try to calculate phi(10090) as an example.
--
-- >>> solution38
-- True
solution38 :: Bool
solution38 = solution34 10090 == solution37 10090
