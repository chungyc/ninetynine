{- |
Description: Truth tables for \(n\)-ary boolean functions
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P48" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P48 (tablen) where

-- | Generalizes 'Problems.P46.table' in a way that the logical expression may contain any number of logical variables.
tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = map (\xs -> xs ++ [f xs]) $ enumerate n

-- | Enumerate all combinations of n boolean values.
enumerate :: Int -> [[Bool]]
enumerate 0 = [[]]
enumerate n = [x : xs | x <- [False, True], xs <- enumerate (n-1)]
