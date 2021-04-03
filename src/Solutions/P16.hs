{- |
Description: Drop elements in a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P16" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P16 (dropEvery) where

-- | Drop every @n@th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery l n = step l 1
  where step [] _ = []
        step (x:xs) k
          | k `mod` n == 0 = step xs (k+1)
          | otherwise      = x : step xs (k+1)
