{- |
Description: 'primeFactors'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P35".
-}
module Problems.P35 (primeFactors) where

import qualified Solutions.P35 as Solution

-- | Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors in ascending order.
--
-- >>> primeFactors 315
-- [3,3,5,7]
--
-- &#129335; &#129335;
primeFactors :: Integral a => a -> [a]
primeFactors = Solution.primeFactors
