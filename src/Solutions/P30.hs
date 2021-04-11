{- |
Description: Fibonacci numbers with matrix exponentiation
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P30" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P30 (fibonacci') where

{- |
Consider the following matrix equation, where \(F(n)\) is the \(n\)th Fibonacci number:

\[
\begin{pmatrix}
x_2 \\
x_1
\end{pmatrix}
=
\begin{pmatrix}
1 & 1 \\
1 & 0
\end{pmatrix}
\begin{pmatrix}
F(n+1) \\
F(n)
\end{pmatrix}
\]

When written out as linear equations, this is equivalent to:

\[
\begin{align}
x_2 & = F(n+1) + F(n) \\
x_1 & = F(n+1)
\end{align}
\]

So \(x_2 = F(n+2)\) and \(x_1 = F(n+1)\).  Together with the associativity of matrix multiplication, this means:

\[
\begin{pmatrix}
F(n+2) \\
F(n+1)
\end{pmatrix}
=
\begin{pmatrix}
1 & 1 \\
1 & 0
\end{pmatrix}
\begin{pmatrix}
F(n+1) \\
F(n)
\end{pmatrix}
=
\begin{pmatrix}
1 & 1 \\
1 & 0
\end{pmatrix}^2
\begin{pmatrix}
F(n) \\
F(n-1)
\end{pmatrix}
= \cdots =
\begin{pmatrix}
1 & 1 \\
1 & 0
\end{pmatrix}^n
\begin{pmatrix}
F(2) \\
F(1)
\end{pmatrix}
\]

Take advantage of this to write a function which computes
the \(n\)th Fibonacci number with \(O(\log n)\) multiplications.
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
