{- |
Description: List of prime factors and their multiplicity
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P36" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P36 (primeFactorsMultiplicity) where

import           Data.List    (genericLength, group)
import           Problems.P35

-- | Determine the prime factors of a given positive integer.
--
-- Construct a list containing the prime factors and their multiplicity.
primeFactorsMultiplicity :: Integral a => a -> [(a, a)]
primeFactorsMultiplicity = map (\xs -> (head xs, genericLength xs)) . group . primeFactors
