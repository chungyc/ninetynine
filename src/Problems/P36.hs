{- |
Description: 'primeFactorsMultiplicity'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P36".
-}
module Problems.P36 (primeFactorsMultiplicity) where

import qualified Solutions.P36 as Solution

-- | Determine the prime factors of a given positive integer.
--
-- Construct a list containing the prime factors and their multiplicity.
--
-- >>> primeFactorsMultiplicity 315
-- [(3,2),(5,1),(7,1)]
primeFactorsMultiplicity :: Integral a => a -> [(a, a)]
primeFactorsMultiplicity = Solution.primeFactorsMultiplicity