{- |
Description: Gaussian integer divisibility
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P43" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P43 (gaussianDividesBy) where

import           Data.Complex
import           Solutions.Arithmetic (dividesBy)

{- | A Gaussian integer is a complex number where both the real and imaginary parts are integers.
If \(x\) and \(y\) are Gaussian integers where \(y \neq 0\),
then \(x\) is said to be divisible by \(y\)
if there is a Guassian integer \(z\) such that \(x=yz\).

Determine whether a Gaussian integer is divisible by another.
-}
gaussianDividesBy
  -- | \(x\)
  :: Complex Integer
  -- | \(y\)
  -> Complex Integer
  -- | \(y \mid x\), i.e., whether \(x\) is divisible by \(y\)
  -> Bool
gaussianDividesBy _ (0 :+ 0)        = False
gaussianDividesBy _ (1 :+ 0)        = True
gaussianDividesBy _ (-1 :+ 0)       = True
gaussianDividesBy _ (0 :+ 1)        = True
gaussianDividesBy _ (0 :+ -1)       = True
gaussianDividesBy (a :+ b) (c :+ d) =
  numeratorReal `dividesBy` denominator && numeratorImag `dividesBy` denominator
  -- Basically normalizing x / y = (x * conjugate y) / (y * conjugate y)
  -- so that the denominator is purely real.
  where numeratorReal = a * c + b * d
        numeratorImag = b * c - a * d
        denominator = c*c + d*d
