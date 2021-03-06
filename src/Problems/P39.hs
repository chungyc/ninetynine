{- |
Description: 'primes'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P39".
-}
module Problems.P39 (primesR, primes) where

import qualified Solutions.P39 as Solution

-- | Given a range of integers by its lower and upper limit (inclusive),
-- construct a list of all prime numbers in that range.
--
-- === Examples
--
-- >>> primesR 10 20
-- [11,13,17,19]
primesR :: Integral a => a -> a -> [a]
primesR = Solution.primesR

-- | Construct the list of all prime numbers.
--
-- === Examples
--
-- >>> take 5 primes
-- [2,3,5,7,11]
primes :: Integral a => [a]
primes = Solution.primes
