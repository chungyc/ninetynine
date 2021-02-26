{- |
Description: 'totient''

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P37".
-}
module Problems.P37 (totient') where

import qualified Solutions.P37 as Solution

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
--
-- Example:
--
-- >>> totient' 10
-- 4
--
-- Use the benchmarks "P34Bench" and "P37Bench" to compare
-- the implementations for "Problems.P34" and "Problems.P37".
totient' :: Integral a => a -> a
totient' = Solution.totient'
