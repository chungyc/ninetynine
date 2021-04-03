{- |
Description: Indexed element in a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P03" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P03 (elementAt) where

-- | Find the @k@th element of a list.
-- The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt [] _ = undefined
elementAt (x:xs) k
  | k == 1    = x
  | k > 1     = elementAt xs (k-1)
  | otherwise = undefined
