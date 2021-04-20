{- |
Description: Fibonacci numbers with matrix exponentiation
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P30".
-}
module Problems.P30 (fibonacci') where

import qualified Solutions.P30 as Solution

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

Compare with the solution for "Problems.P29":

> $ stack bench --benchmark-arguments="P29/fibonacci P30/fibonacci'"

=== Examples

>>> map fibonacci' [1..10]
[1,1,2,3,5,8,13,21,34,55]

>>> fibonacci' 1000000
19532821287077577316...

>>> length $ show $ fibonacci' 1000000
208988

=== __Hint__

With exponentiation by squaring, \(x^n\) can be computed with \(O(\log n)\) multiplications.
E.g., since \(x^{39} = (((((((x^2 )^2 )^2) x)^2) x)^2 ) x\), \(x^{39}\) can be computed with 8 multiplications instead of 38.
-}
fibonacci' :: Integral a => a -> a
fibonacci' = Solution.fibonacci'
