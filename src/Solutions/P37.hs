{- |
Description: Euler's totient function with Euler's product formula
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P37" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P37 (totient') where

import           Problems.P36

-- | Calculate Euler's totient function \(\phi(m)\).
--
-- See 'Problems.P34' for the definition of Euler's totient function.
-- Implement an improved version based on Euler's product formula.
--
-- If the prime factors of \(m\) are known, i.e.,
--
-- \[ m = \prod_{i=1}^r {p_i}^{k_i} \]
--
-- where \(p_i\) is prime and \(k_i \geq 1\), then
--
-- \[ \phi(m) = \prod_{i=1}^r {p_i}^{k_i - 1} (p_i - 1) \]
totient' :: Integral a => a -> a
totient' m = product $ map (\(p, k) -> p^(k-1) * (p-1)) factors
  where factors = primeFactorsMultiplicity m
