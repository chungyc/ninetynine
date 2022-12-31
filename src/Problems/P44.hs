{- |
Description: Gaussian primes
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P44".
-}
module Problems.P44 (isGaussianPrime) where

import           Data.Complex
import qualified Solutions.P44 as Solution

isGaussianPrime :: Complex Integer -> Bool
isGaussianPrime = Solution.isGaussianPrime
