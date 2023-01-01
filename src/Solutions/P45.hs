{- |
Description: Gaussian primes using the two-square theorem
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P45" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P45 (isGaussianPrime') where

import           Data.Complex
import           Problems.P31

{- | Using Fermat's two-square theorem, it can be shown that a Gaussian integer \(a+bi\) is prime
if and only if it falls into one of the following categories:

* |a| is prime and \(|a| \equiv 3 \mod 4\), if \(b=0\).
* |b| is prime and \(|b| \equiv 3 \mod 4\), if \(a=0\).
* \( a^2 + b^2 \) is prime, if \( a \neq 0 \) and \( b \neq 0 \).

Use this property to determine whether a Gaussian integer is a Gaussian prime.
-}
isGaussianPrime' :: Complex Integer -> Bool
isGaussianPrime' (a :+ 0) = abs a `mod` 4 == 3 && isPrime (abs a)
isGaussianPrime' (0 :+ b) = abs b `mod` 4 == 3 && isPrime (abs b)
isGaussianPrime' (a :+ b) = isPrime (a*a + b*b)
