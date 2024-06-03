{- |
Description: Gaussian primes
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P44" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P44 (isGaussianPrime) where

import           Data.Complex
import           Data.List            ((\\))
import           Problems.P43         (gaussianDividesBy)
import           Solutions.Arithmetic (gaussianMultiply, gaussianUnits)

-- | Determine whether a Gaussian integer is a Gaussian prime.
isGaussianPrime :: Complex Integer -> Bool
isGaussianPrime (0 :+ 0) = False
isGaussianPrime (1 :+ 0) = False
isGaussianPrime ((-1) :+ 0) = False
isGaussianPrime (0 :+ 1) = False
isGaussianPrime (0 :+ (-1)) = False
isGaussianPrime x = null $ divisors \\ exclusions
  where divisors = filter (gaussianDividesBy x) $ candidates x
        exclusions = associates ++ gaussianUnits
        associates = map (gaussianMultiply x) gaussianUnits

candidates :: Complex Integer -> [Complex Integer]
candidates (a :+ b) = [ x :+ y  | x <- [0..bound], y <- [0..bound] ]
  where norm = square a + square b
        bound = until (\x -> (square . square) x >= norm) (+1) 1
        square x = x*x
