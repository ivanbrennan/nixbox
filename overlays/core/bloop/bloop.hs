module Main where

import Test.QuickCheck.Gen (Gen, choose, generate)

main :: IO ()
main = print =<< generate int
  where
    int :: Gen Int
    int = choose (0, 9)
