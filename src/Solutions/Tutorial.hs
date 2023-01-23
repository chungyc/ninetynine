{- |
Description: Tutorial for solving problems
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.Tutorial".
-}
module Solutions.Tutorial (sumNumbers, sumNumbers', sumNumbers'') where

-- | Add numbers from 1 to a given number @n@.
sumNumbers :: Integer -> Integer
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n-1)

-- | Add numbers from 1 to a given number @n@.
--
-- Accumulates the sum with a tail recursive function.
sumNumbers' :: Integer -> Integer
sumNumbers' n | n >= 0 = add 0 n
              | otherwise = 0
  where add s 0 = s
        add s m = add (s+m) (m-1)

-- | Add numbers from 1 to a given number @n@.
--
-- Computes \( \frac{n (n+1)}{2} \).
sumNumbers'' :: Integer -> Integer
sumNumbers'' n | n >= 0 = n * (n+1) `div` 2
               | otherwise = 0
