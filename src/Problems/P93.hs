{- |
Description: An arithmetic puzzle
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P93".
-}
module Problems.P93 (arithmeticPuzzle) where

import qualified Solutions.P93 as Solution

-- $setup
-- >>> import Data.List (sort)

{- |
Given a list of positive integer numbers, find a correct way of inserting
the arithmetic signs such that the result is a correct equation.
For example, with the list of numbers @[2,3,5,7,11]@,
we can form the equations @2-3+5+7=11@ or @2=(3*5+7)/11@.

The arithmetic signs to insert are:

* @+@ : addition
* @-@ : subtraction
* @*@ : multiplication
* @/@ : division
* @=@ : equality
* @(@, @)@ : parentheses

Arithmetic operations are only binary, e.g., @-4@ should not be included.
Division should be interpreted as operating on rationals,
e.g., \(3/5 = 6/10\) but \(3/5 \neq 0\), and division by zero should be avoided.
Parentheses should be inserted only when the default precedence rules need to be overridden.
Equality should be inserted exactly once.

=== Examples

>>> mapM_ putStrLn $ sort $ arithmeticPuzzle [2,3,5,7,11]
2 = (3*5+7)/11
2 = 3-(5+7)+11
2 = 3-(5+7-11)
2 = 3-5-(7-11)
2 = 3-5-7+11
2*(3-5) = 7-11
2-(3-(5+7)) = 11
2-(3-5)+7 = 11
2-(3-5-7) = 11
2-3+5+7 = 11
-}
arithmeticPuzzle :: [Integer] -> [String]
arithmeticPuzzle = Solution.arithmeticPuzzle
