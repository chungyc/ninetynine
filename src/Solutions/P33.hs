{- |
Description: Coprimality
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P33" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P33 (coprime) where

import           Problems.P32

-- | Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Integral a => a -> a -> Bool
coprime a b = myGCD a b == 1
