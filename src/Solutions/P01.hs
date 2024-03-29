{- |
Description: Last element of a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P01" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P01 (myLast) where

-- | Find the last element of a list.
myLast :: [a] -> Maybe a
myLast [x]    = Just x
myLast (_:xs) = myLast xs
myLast _      = Nothing
