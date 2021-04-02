{- |
Description: 'combinations'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P26" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P26 (combinations) where

{- |
Generate the combinations of \(k\) distinct objects chosen from the \(n\) elements of a list.

In how many ways can a committee of 3 be chosen from a group of 12 people?
We all know that there are \({12 \choose 3} = 220\) possibilities,
where \(n \choose k\) denotes the binomial coefficient.
For pure mathematicians, this result may be great.
But we want to really generate all the possibilities in a list.
-}
combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations 1 [x]    = [[x]]
combinations n (x:xs) = combinations n xs ++ map (x:) (combinations (n-1) xs)
