{- |
Description: Goldbach's conjecture
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P40" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P40 (goldbach) where

import           Problems.P31
import           Solutions.Arithmetic

-- | Find two prime numbers that sum up to a given even integer.
goldbach :: Integral a => a -> (a,a)
goldbach n = search n primes

search :: Integral a => a -> [a] -> (a, a)
search n (p:ps)
  | p >= n    = undefined
  | isPrime q = (p, q)
  | otherwise = search n ps
  where q = n-p
search _ _ = undefined
