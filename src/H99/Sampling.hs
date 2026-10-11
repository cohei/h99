module H99.Sampling (discreteUniform) where

import System.Random (randomRIO)

discreteUniform :: [a] -> IO a
discreteUniform xs = (xs !!) <$> randomRIO (0, length xs - 1)
