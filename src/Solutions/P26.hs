{- |
Description: Combinations
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P26" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P26 (combinations) where

{- |
Generate the combinations of \(k\) distinct objects chosen from the \(n\) elements of a list.
-}
combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations 1 [x]    = [[x]]
combinations n (x:xs) = combinations n xs ++ map (x:) (combinations (n-1) xs)
