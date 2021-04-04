{- |
Description: Support functions for arithmetic problems.
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

These functions were written in support of specific arithmetic problems,
but which turn out to be useful for other problems as well.
-}
module Solutions.Arithmetic (dividesBy, primes) where

-- | Whether the first argument divides by the second argument.
--
-- Initially written to support "Problems.P31".
dividesBy :: Integral a => a -> a -> Bool
dividesBy a b = a `mod` b == 0

-- | List of all prime numbers.
--
-- Computed with an Erastothenes sieve.  Unlike the classic sieve,
-- which strikes out multiples of prime numbers from subsequent numbers,
-- checks the primality of each integer against the prime numbers already determined.
--
-- Initially written to support "Problems.P31".
primes :: Integral a => [a]
primes = 2 : odds
  where odds = iterate (next . (2+)) 3
        next n
          | any (n `dividesBy`) (candidates n) = next $ n+2
          | otherwise                          = n
        candidates n = takeWhile (\k -> k*k <= n) odds
