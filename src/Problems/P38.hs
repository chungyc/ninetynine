{- |
Description: Highly totient numbers
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P38".
-}
module Problems.P38 (highlyTotientNumbers) where

import qualified Solutions.P38 as Solution

{- |
It is possible for more than one number \(x\) to have the same totient number \(\phi(x)\).
For example, \(\phi(77) = \phi(93) = 60\).

A highly totient number \(n\) has the most such \(x\)
among the totient numbers less than or equal to \(n\).
I.e., \(n\) is a highly totient number
if \(|\{x \,|\, \phi(x)=n \}| > |\{x \,|\, \phi(x)=m \}|\) for \(m < n\).

Construct the list of highly totient numbers.

=== Examples

>>> take 10 highlyTotientNumbers
[1,2,4,8,12,24,48,72,144,240]

=== __Hint__

Given a totient number \(n=\phi(x)\), find an upper bound for \(x\),
which will bound the set of numbers from which to search for solutions.
Compare the prime factorizations between \(\phi(x)\) and \(x\),
and devise a modification of the former so that it must be larger than the latter.
-}
highlyTotientNumbers :: Integral a => [a]
highlyTotientNumbers = Solution.highlyTotientNumbers
