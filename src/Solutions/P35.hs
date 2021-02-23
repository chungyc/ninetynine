{- |
Description: 'primeFactors'

Some solutions to "Problems.P35" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P35 (primeFactors) where

import           Solutions.P31.Functions

-- | Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors in ascending order.
primeFactors :: Integral a => a -> [a]
primeFactors n = reverse $ factor n primes []

factor :: Integral a => a -> [a] -> [a] -> [a]
factor 1 _ fs = fs
factor n ps'@(p:ps) fs
  | n `dividesBy` p = factor (n `div` p) ps' (p:fs)
  | otherwise       = factor n ps fs
factor _ _ _ = undefined