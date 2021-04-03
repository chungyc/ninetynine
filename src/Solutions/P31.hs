{- |
Description: Primality checking
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P31" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P31 (isPrime, isPrime', isPrime'') where

import           Solutions.P31.Functions

-- | Determine whether a given integer number is prime.
--
-- Checks whether the integer is even or if there is a divisor
-- among odd integers not greater than its square root.
isPrime :: Integral a => a -> Bool
isPrime n
  | n == 2         = True
  | n > 2          = null $ filter (n `dividesBy`) $ 2 : odds
  | otherwise      = False
  where odds = takeWhile (\k -> k*k <= n) $ iterate (2+) 3

-- | Determine whether a given integer number is prime.
--
-- Uses an Erastothenes sieve to construct a list of primes up
-- to at least the square root of the integer, and searches for
-- a divisor among them.
isPrime' :: Integral a => a -> Bool
isPrime' n
  | n == 2    = True
  | even n    = False
  | n > 2     = not $ any (n `dividesBy`) $ ps
  | otherwise = False
  where ps = sieve (takeWhile (\k -> k*k <= n) $ iterate (2+) 3)

sieve :: Integral a => [a] -> [a]
sieve []     = []
sieve (n:ns) = n : sieve (filter (not . dividesBy n) ns)

-- | Determine whether a given integer number is prime.
--
-- From a list of all prime numbers, search for a divisor.
isPrime'' :: Integral a => a -> Bool
isPrime'' n
  | n > 1     = not $ any (n `dividesBy`) $ takeWhile (\k -> k*k <= n) primes
  | otherwise = False
