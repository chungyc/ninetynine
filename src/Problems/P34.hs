{- |
Description: 'totient'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P34".
-}
module Problems.P34 (totient) where

import qualified Solutions.P34 as Solution

-- | Calculate Euler's totient function \(\phi(m)\).
--
-- Euler's so-called totient function \(\phi(m)\) is defined as
-- the number of positive integers \(r\), where \(1 \leq r \leq m\), that are coprime to \(m\).
--
-- For example, with \(m = 10\), \(\{r \,|\, 1 \leq r \leq m, \textrm{coprime to $m$}\} = \{ 1, 3, 7, 9 \}\);
-- thus \(\phi(m) = 4\).  Note the special case of \(\phi(1) = 1\).
--
-- Example in Haskell:
--
-- >>> totient 10
-- 4
totient :: Integral a => a -> a
totient = Solution.totient
