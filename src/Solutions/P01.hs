{- |
Description: 'myLast'

Some solutions to "Problems.P01" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P01 (myLast) where

-- | Find the last element of a list.
myLast :: [a] -> a
myLast [x]    = x
myLast (_:xs) = myLast xs
myLast _      = undefined
