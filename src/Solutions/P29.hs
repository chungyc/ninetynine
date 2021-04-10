{- |
Description: Fibonacci numbers
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P29" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P29 (fibonacci) where

{- |
For \(n > 2\), the \(n\)th Fibonacci number \(F(n)\) is the sum of \(F(n-1)\) and \(F(n-2)\),
and the first and second Fibonacci numbers are 1.  I.e.,

\[
\begin{align}
F(1) & = 1 \\
F(2) & = 2 \\
F(n) & = F(n-1) + F(n-2)
\end{align}
\]

Write a function to compute the \(n\)th Fibonacci number.
-}
fibonacci :: Integral a => a -> a
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n | n > 2     = fib 1 1 n
            | otherwise = undefined

fib :: Integral a => a -> a -> a -> a
fib k k' 3 = k + k'
fib k k' n = fib k' (k+k') (n-1)
