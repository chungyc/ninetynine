{- |
Description: List monad
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P77" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P77 (randomWalkPaths) where

{- | A list is also a monad.

For example, "Problems.P14" could have been implemented with the list monad:

>>> :{
dupli :: [a] -> [a]
dupli xs = do
  x <- xs
  [x, x]
:}

>>> dupli [1, 2, 3]
[1,1,2,2,3,3]

Using the list monad, implement a function which returns
all the one-dimensional random walk paths with \(n\) steps.
Starting from position 0, each step one can change positions by -1, 0, or 1.
Each path will be a list of positions starting from 0.
-}
randomWalkPaths :: Int -> [[Int]]
randomWalkPaths n = map reverse $ randomWalkPaths' n

-- Returns the random walk paths, except the paths are in reverse order.
randomWalkPaths' :: Int -> [[Int]]
randomWalkPaths' 0 = [[0]]
randomWalkPaths' n = do
  path <- randomWalkPaths' (n-1)
  step <- [-1,0,1]
  return (path' path step)
  where path' []       _ = []
        path' xs@(x:_) s = (x+s):xs
