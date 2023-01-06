{- |
Description: Fibonacci numbers with matrix exponentiation
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P30" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P30 (fibonacci') where

{- |
Computes the \(n\)th Fibonacci number with \(O(\log n)\) multiplications.
Takes advantage of matrix multiplication and exponentiation.
-}
fibonacci' :: Integral a => a -> a
fibonacci' 1 = 1
fibonacci' 2 = 1
fibonacci' n | n > 2     = a+b
             | otherwise = undefined
  where ((a,b), _) = power ((1,1),(1,0)) (n-2)

type Matrix a = ((a,a),
                 (a,a))

power :: Integral a => Matrix a -> a -> Matrix a
power base 1 = base
power base n | even n    = multiply x x
             | otherwise = multiply base $ power base (n-1)
  where x = power base (n `div` 2)

multiply :: Integral a => Matrix a -> Matrix a -> Matrix a
multiply
  ( (a, b)
  , (c, d)
  )
  ( (a', b')
  , (c', d')
  )
  = ( (a*a'+b*c', a*b'+b*d')
    , (c*a'+d*c', c*b'+d*d')
    )
