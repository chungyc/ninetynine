{- |
Description: 'totient'

Some solutions to "Problems.P34" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P34 (totient) where

import           Problems.P33

-- | Calculate Euler's totient function \(\phi(m)\).
--
-- Euler's so-called totient function \(\phi(m)\) is defined as
-- the number of positive integers \(r\), where \(1 \leq r \leq m\), that are coprime to \(m\).
--
-- For example, with \(m = 10\), \(\{r \,|\, 1 \leq r \leq m, \textrm{coprime to $m$}\} = \{ 1, 3, 7, 9 \}\);
-- thus \(\phi(m) = 4\).  Note the special case of \(\phi(1) = 1\).
totient :: Integral a => a -> a
totient m = accumulate m 0
  where accumulate 0 phi = phi
        accumulate r phi
          | coprime m r = accumulate (r-1) (phi+1)
          | otherwise   = accumulate (r-1) phi
