{- |
Description : Ninety-Nine Haskell Solutions
Maintainer  : dev@chungyc.org

Solutions to the [99 Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).
-}

module Solutions (
  -- * Problem 1
  myLast,
  -- * Problem 2
  myButLast,
  ) where

-- | Find the last element of a list.
myLast :: [a] -> a
myLast [] = undefined
myLast [x] = x
myLast (_:xs) = myLast xs

-- | Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = undefined
myButLast [_] = undefined
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs
