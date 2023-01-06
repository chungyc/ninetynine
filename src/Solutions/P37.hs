{- |
Description: Euler's totient function with Euler's product formula
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P37" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P37 (totient') where

import           Problems.P36

-- | Calculate Euler's totient function \(\phi(m)\) using Euler's product formula.
totient' :: Integral a => a -> a
totient' m = product $ map (\(p, k) -> p^(k-1) * (p-1)) factors
  where factors = primeFactorsMultiplicity m
