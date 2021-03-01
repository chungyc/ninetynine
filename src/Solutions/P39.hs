{- |
Description: 'primes'

Some solutions to "Problems.P39" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P39 (primesR, primes) where

import qualified Solutions.P31.Functions as Functions

-- | Given a range of integers by its lower and upper limit (inclusive),
-- construct a list of all prime numbers in that range.
primesR :: Integral a => a -> a -> [a]
primesR lo hi = takeWhile (<= hi) $ dropWhile (< lo) primes

-- | Construct the list of all prime numbers.
primes :: Integral a => [a]
primes = Functions.primes  -- Already done as part of a solution to problem 31.
