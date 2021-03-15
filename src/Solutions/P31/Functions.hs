{- |
Description: Support functions for "Problem.P31".

These functions were written in support of "Problem.P31",
but which turn out to be useful for other problems as well.
-}
module Solutions.P31.Functions (dividesBy, primes) where

-- | Whether the first argument divides by the second argument.
dividesBy :: Integral a => a -> a -> Bool
dividesBy a b = a `mod` b == 0

-- | List of all prime numbers.
--
-- Computed with an Erastothenes sieve.  Unlike the classic sieve,
-- which strikes out multiples of prime numbers from subsequent numbers,
-- checks the primality of each integer against the prime numbers already determined.
primes :: Integral a => [a]
primes = 2 : odds
  where odds = iterate (next . (2+)) 3
        next n
          | any (n `dividesBy`) (candidates n) = next $ n+2
          | otherwise                          = n
        candidates n = takeWhile (\k -> k*k <= n) odds
