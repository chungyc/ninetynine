{- |
Description: Fibonacci numbers
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P29".
-}
module Problems.P29 (fibonacci) where

import qualified Solutions.P29 as Solution

{- |
For \(n > 2\), the \(n\)th Fibonacci number \(F(n)\) is the sum of \(F(n-1)\) and \(F(n-2)\),
and the first and second Fibonacci numbers are 1.  I.e.,

\[
\begin{align}
F(1) & = 1 \\
F(2) & = 1 \\
F(n) & = F(n-1) + F(n-2)
\end{align}
\]

Write a function to compute the \(n\)th Fibonacci number.

=== Examples

>>> map fibonacci [1..10]
[1,1,2,3,5,8,13,21,34,55]
-}
fibonacci :: Integral a => a -> a
fibonacci = Solution.fibonacci
