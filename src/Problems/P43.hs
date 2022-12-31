{- |
Description: Gaussian integer divisibility
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P43".
-}
module Problems.P43 (gaussianDividesBy) where

import           Data.Complex
import qualified Solutions.P43 as Solution

{- | A Gaussian integer is a complex number where both the real and imaginary parts are integers.
If \(x\) and \(y\) are Gaussian integers where \(y \neq 0\),
then \(x\) is said to be divisible by \(y\)
if there is a Guassian integer \(z\) such that \(x=yz\).

Determine whether a Gaussian integer is divisible by another.

=== Examples

\(10 = 2 \times 5\), so

>>> (10 :+ 0) `gaussianDividesBy` (2 :+ 0)
True

\(10 = -2i \times 5i\), so

>>> (10 :+ 0) `gaussianDividesBy` (0 :+ 2)
True

However, there is no Gaussian integer \(x\) such that \(5+2i = (2-i)x\), so

>>> (5 :+ 2) `gaussianDividesBy` (2 :+ (-1))
False

=== __Hint__

For \(y \neq 0\), \(z = xy\) means that \(x = \frac{z}{y}\).
If you multiply both the numerator and denominator by \(\overline{y}\),
the conjugate of \(y\), then you can normalize the denominator to be a real number.
You can then use this to check whether the real and imaginary parts are integers.
Recall that \(\overline{a+bi} = a-bi\) and \( (a+bi)(c+di) = (ac-bd) + (bc+ad)i \)
for real numbers \(a\), \(b\), \(c\), and \(d\).
-}
gaussianDividesBy
  -- | \(x\)
  :: Complex Integer
  -- | \(y\)
  -> Complex Integer
  -- | \(y \mid x\), i.e., whether \(x\) is divisible by \(y\)
  -> Bool
gaussianDividesBy = Solution.gaussianDividesBy
