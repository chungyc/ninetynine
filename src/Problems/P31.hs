{- |
Description: Primality checking
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P31".
-}
module Problems.P31 (isPrime) where

import qualified Solutions.P31 as Solution

-- | Determine whether a given integer number is prime.
--
-- === Examples
--
-- >>> isPrime 7
-- True
--
-- >>> isPrime 15
-- False
isPrime :: Integral a => a -> Bool
isPrime = Solution.isPrime
