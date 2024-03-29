{- |
Description: List of prime factors
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P35".
-}
module Problems.P35 (primeFactors) where

import qualified Solutions.P35 as Solution

-- | Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors in ascending order.
--
-- === Examples
--
-- >>> primeFactors 315
-- [3,3,5,7]
primeFactors :: Integral a => a -> [a]
primeFactors = Solution.primeFactors
