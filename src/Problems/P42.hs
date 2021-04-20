{- |
Description: Modular multiplicative inverse
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P42".
-}
module Problems.P42 (multiplicativeInverse) where

import qualified Solutions.P42 as Solution

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

=== Examples

>>> multiplicativeInverse 3 5
Just 2

>>> multiplicativeInverse 48 127
Just 45

>>> multiplicativeInverse 824 93
Just 50

>>> multiplicativeInverse 48 93
Nothing
-}
multiplicativeInverse :: Integral a => a -> a -> Maybe a
multiplicativeInverse = Solution.multiplicativeInverse
