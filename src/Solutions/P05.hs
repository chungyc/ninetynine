{- |
Description: 'myReverse'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P05" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P05 (myReverse) where

-- | Reverse a list.
myReverse :: [a] -> [a]
myReverse l = accumulate l []

-- | Accumulates into the partial reversed list in the second argument
-- as extracted from the remainder of the original list in the first argument.
--
-- Returns the fully accumulated reversed list.
accumulate :: [a] -> [a] -> [a]
accumulate [] xs     = xs
accumulate (x:xs) ys = accumulate xs (x:ys)
