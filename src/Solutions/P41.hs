{- |
Description: List of Goldbach pairs
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P41" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P41 (goldbachList) where

import           Problems.P40

-- | Given a range of integers by its lower and upper limit,
-- return a list of all Goldbach compositions for the even numbers in the range.
--
-- In most cases, if an even number is written as the sum of two prime numbers, one of them is very small.
-- Very rarely, the primes are both bigger than say 50.
-- Try to find out how many such cases there are in the range \([2,3000]\).
goldbachList :: Integral a => a -> a -> [(a,a)]
goldbachList m n = map goldbach $ takeWhile (n >=) [m',m'+2..]
  where m' | m <= 2         = 4
           | m `mod` 2 == 0 = m
           | otherwise      = m+1
