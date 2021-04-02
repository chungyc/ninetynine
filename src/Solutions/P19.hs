{- |
Description: 'rotate'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P19" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P19 (rotate) where

-- | Rotate a list @n@ places to the left.
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n = drop shift xs ++ take shift xs
  where m = length xs
        modShift = n `mod` m
        shift
          | modShift < 0 = m - modShift
          | otherwise    = modShift
