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

{- | A Gaussian integer \(x\) is said to be a /Gaussian prime/ when it has no divisors except for
the units and associates of \(x\).  The units are \(1\), \(i\), \(-1\), and \(-i\).
The associates are defined by the numbers obtained when \(x\) is multiplied by each unit.

Determine whether a Gaussian integer is a Gaussian prime.

=== Examples

Since \(5i = (2+i)(1+2i)\),

>>> isGaussianPrime (0 :+ 5)
False

Since \(17 = (4+i)(4-i)\),

>>> isGaussianPrime (17 :+ 0)
False

No non-unit proper divisors divide \(5+2i\) or \(-2+5i\), so

>>> isGaussianPrime (5 :+ 2)
True
>>> isGaussianPrime ((-2) :+ 5)
True

=== __Hint__

Recall that \(|xy| = |x| \, |y|\) for complex \(x\) and \(y\).
This implies that for any proper divisor \(y\) of a Gaussian integer \(x\) where \(|x| > 1\),
it will be the case that \(|y| < |x|\).  In fact, it will be the case that there will
be a non-unit divisor \(z\) such that \(|z|^2 \leq |x|\), if any non-unit proper divisor exists.
-}
isGaussianPrime :: Complex Integer -> Bool
isGaussianPrime = Solution.isGaussianPrime
