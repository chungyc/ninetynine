{- |
Description: Modular multiplicative inverse
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P42" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P42 (multiplicativeInverse) where

{- |
Compute the multiplicative inverse \(x\) of a given integer \(a\) and modulus \(n\)
lying in the range \(0 \leq x < n\).

Uses the [extended Euclidean algorithm](https://brilliant.org/wiki/extended-euclidean-algorithm/)
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
