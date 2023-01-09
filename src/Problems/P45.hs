{- |
Description: Gaussian primes using the two-square theorem
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P45".
-}
module Problems.P45 (isGaussianPrime') where

import           Data.Complex
import qualified Solutions.P45 as Solution

{- | Using Fermat's two-square theorem, it can be shown that a Gaussian integer \(a+bi\) is prime
if and only if it falls into one of the following categories:

* |a| is prime and \(|a| \equiv 3 \mod 4\), if \(b=0\).
* |b| is prime and \(|b| \equiv 3 \mod 4\), if \(a=0\).
* \( a^2 + b^2 \) is prime, if \( a \neq 0 \) and \( b \neq 0 \).

Use this property to determine whether a Gaussian integer is a Gaussian prime.

Compare with the solution for "Problems.P44":

> $ stack bench --benchmark-arguments="P44/isGaussianPrime P45/isGaussianPrime'"

=== Examples

>>> isGaussianPrime' (0 :+ 5)
False
>>> isGaussianPrime' (17 :+ 0)
False
>>> isGaussianPrime' (5 :+ 2)
True
>>> isGaussianPrime' ((-2) :+ 5)
True
-}
isGaussianPrime' :: Complex Integer -> Bool
isGaussianPrime' = Solution.isGaussianPrime'
