module Main where

import           Data.Numbers.Divisors
import           Data.Numbers.Primes

main :: IO ()
main = do
  print (divisors 62947382938)
  interact $
    unlines . map show
    . map divisors
    . map read . lines
