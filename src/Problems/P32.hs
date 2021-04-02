{- |
Description: 'myGCD'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P32".
-}
module Problems.P32 (myGCD) where

import qualified Solutions.P32 as Solution

-- | Determine the greatest common divisor of two positive integer numbers.
-- Use [Euclid's algorithm](https://en.wikipedia.org/wiki/Euclidean_algorithm).
--
-- === Examples
--
-- >>> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]
myGCD :: Integral a => a -> a -> a
myGCD = Solution.myGCD
