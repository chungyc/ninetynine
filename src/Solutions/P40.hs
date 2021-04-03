{- |
Description: Goldbach's conjecture
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P40" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P40 (goldbach) where

import           Problems.P31
import           Solutions.P31.Functions

-- | Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
-- For example: \(28 = 5 + 23\).
-- It is one of the most famous facts in number theory that has not been proved to be correct in the general case.
-- It has been numerically confirmed up to very large numbers.
--
-- Write a function to find the two prime numbers that sum up to a given even integer.
goldbach :: Integral a => a -> (a,a)
goldbach n = search n primes

search :: Integral a => a -> [a] -> (a, a)
search n (p:ps)
  | p >= n    = undefined
  | isPrime q = (p, q)
  | otherwise = search n ps
  where q = n-p
search _ _ = undefined
