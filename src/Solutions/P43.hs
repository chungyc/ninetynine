{- |
Description: Gaussian integer divisibility
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P43" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P43 (gaussianDividesBy) where

import           Data.Complex
import           Solutions.Arithmetic (dividesBy, gaussianMultiply)

-- | Determine whether a Gaussian integer is divisible by another.
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
gaussianDividesBy x y@(c :+ d) =
  realPart numerator `dividesBy` denominator && imagPart numerator `dividesBy` denominator
  -- Normalizing x / y = (x * conjugate y) / (y * conjugate y)
  -- so that the denominator is purely real.
  where numerator = gaussianMultiply x $ conjugate y
        denominator = c*c + d*d
