{- |
Description: List monad
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P77" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P77 (randomWalkPaths) where

-- | Returns all the one-dimensional random walk paths with \(n\) steps
-- starting from position 0.
randomWalkPaths :: Int -> [[Int]]
randomWalkPaths n = map reverse $ randomWalkPaths' n

-- Returns the random walk paths, except the paths are in reverse order.
randomWalkPaths' :: Int -> [[Int]]
randomWalkPaths' 0 = [[0]]
randomWalkPaths' n = do
  path <- randomWalkPaths' (n-1)
  step <- [-1,0,1]
  let path' = extend path step
  return path'
  where extend []       _ = []
        extend xs@(x:_) s = (x+s):xs
