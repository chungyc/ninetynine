{- |
Description: Gaussian primes
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P44" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P44 (isGaussianPrime) where

import           Data.Complex
import           Data.List    ((\\))
import           Problems.P43 (gaussianDividesBy)

{- | A Gaussian integer \(x\) is said to be a /Gaussian prime/ when it has no divisors except for
the units and associates of \(x\).  The units are \(1\), \(i\), \(-1\), and \(-i\).
The associates are defined by the numbers obtained when \(x\) is multiplied by each unit.

Determine whether a Gaussian integer is a Gaussian prime.
-}
isGaussianPrime :: Complex Integer -> Bool
isGaussianPrime (0 :+ 0) = False
isGaussianPrime (1 :+ 0) = False
isGaussianPrime ((-1) :+ 0) = False
isGaussianPrime (0 :+ 1) = False
isGaussianPrime (0 :+ (-1)) = False
isGaussianPrime x = null $ divisors \\ exclusions
  where divisors = filter (gaussianDividesBy x) $ candidates x
        exclusions = associates ++ units
        associates = map (multiply x) units

candidates :: Complex Integer -> [Complex Integer]
candidates (a :+ b) = [ x :+ y  | x <- [0..bound], y <- [0..bound] ]
  where norm = square a + square b
        bound = head $ dropWhile (\x -> (square . square) x < norm) [1..]
        square x = x*x

units :: [Complex Integer]
units = [ 1:+0, (-1):+0, 0:+1, 0:+(-1) ]

multiply :: Complex Integer -> Complex Integer -> Complex Integer
multiply (a :+ b) (c :+ d) = (a*c-b*d) :+ (a*d+b*c)
