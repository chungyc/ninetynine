{- |
Description: Split a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P17" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P17 (split) where

-- | Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split [] _     = ([], [])
split xs 0     = ([], xs)
split (x:xs) n = let (ys, zs) = split xs (n-1) in (x : ys, zs)
