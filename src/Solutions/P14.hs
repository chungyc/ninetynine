{- |
Description: Duplicate elements in a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P14" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P14 (dupli) where

-- | Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x : x : dupli xs
