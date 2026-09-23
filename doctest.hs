module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-XGHC2024", "H99.hs"]
