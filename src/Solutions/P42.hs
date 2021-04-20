{- |
Description: Modular multiplicative inverse
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P42" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P42 (multiplicativeInverse) where

{- |
In [modular arithmetic](https://brilliant.org/wiki/modular-arithmetic/),
integers \(a\) and \(b\) being congruent modulo an integer \(n\),
also written as \(a \equiv b \pmod{n}\), means that \(a - b = kn\) for some integer \(k\).
Many of the usual rules for addition, subtraction, and multiplication in ordinary arithmetic
also hold for modular arithmetic.

A multiplicative inverse of an integer \(a\) modulo \(n\) is an integer \(x\) such that \(ax \equiv 1 \pmod{n}\).
It exists if and only if \(a\) and \(n\) are coprime.

Write a function to compute the multiplicative inverse \(x\) of a given integer \(a\) and modulus \(n\)
lying in the range \(0 \leq x < n\).
Use the [extended Euclidean algorithm](https://brilliant.org/wiki/extended-euclidean-algorithm/)
and the fact that \(ax \equiv 1 \pmod{n}\) if \(ax+ny=1\).
-}
multiplicativeInverse :: Integral a => a -> a -> Maybe a
multiplicativeInverse a n
  | a >= n    = multiplicativeInverse (a `mod` n) n
  | r == 1    = Just $ x `mod` n
  | otherwise = Nothing
  where (r,x,_) = reduce (n,a) (0,1) (1,0)

reduce :: Integral a => (a,a) -> (a,a) -> (a,a) -> (a,a,a)
reduce (0,r') (_,x') (_,y')   = (r',x',y')
reduce (r,r') (x,x') (y,y') = reduce (r'-q*r, r) (x'-q*x, x) (y'-q*y, y)
  where q = r' `div` r
