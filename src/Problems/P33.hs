{- |
Description: Coprimality
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P33".
-}
module Problems.P33 (coprime) where

import qualified Solutions.P33 as Solution

-- | Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1.
--
-- === Examples
--
-- >>> coprime 35 64
-- True
--
-- >>> coprime 1173 1547
-- False
coprime :: Integral a => a -> a -> Bool
coprime = Solution.coprime
