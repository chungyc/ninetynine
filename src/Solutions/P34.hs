{- |
Description: Euler's totient function
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P34" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P34 (totient,totientFiltered) where

import           Data.List    (genericLength)
import           Problems.P33

-- | Calculate Euler's totient function \(\phi(m)\).
--
-- Euler's so-called totient function \(\phi(m)\) is defined as
-- the number of positive integers \(r\), where \(1 \leq r \leq m\), that are coprime to \(m\).
--
-- For example, with \(m = 10\), \(\{r \,|\, 1 \leq r \leq m, \textrm{coprime to $m$}\} = \{ 1, 3, 7, 9 \}\);
-- thus \(\phi(m) = 4\).  Note the special case of \(\phi(1) = 1\).
--
-- Accumulates count of numbers that are coprime.
totient :: Integral a => a -> a
totient m = accumulate m 0
  where accumulate 0 phi = phi
        accumulate r phi
          | coprime m r = accumulate (r-1) (phi+1)
          | otherwise   = accumulate (r-1) phi

-- | Calculate Euler's totient function \(\phi(m)\).
--
-- Euler's so-called totient function \(\phi(m)\) is defined as
-- the number of positive integers \(r\), where \(1 \leq r \leq m\), that are coprime to \(m\).
--
-- For example, with \(m = 10\), \(\{r \,|\, 1 \leq r \leq m, \textrm{coprime to $m$}\} = \{ 1, 3, 7, 9 \}\);
-- thus \(\phi(m) = 4\).  Note the special case of \(\phi(1) = 1\).
--
-- Filters through numbers that are coprime and count them.
totientFiltered :: Integral a => a -> a
totientFiltered m = genericLength $ filter (coprime m) $ [1..m]
