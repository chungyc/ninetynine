{- |
Description: List monad
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P77".
-}
module Problems.P77 (randomWalkPaths) where

import qualified Solutions.P77 as Solution

-- $setup
-- >>> import Data.List (sort)

{- | A list is also a monad.

For example, 'Problems.P14.dupli' could have been implemented with the list monad:

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
Starting from position 0, each step can change positions by -1, 0, or 1.
Each path will be a list of positions starting from 0.

=== Examples

>>> randomWalkPaths 0
[[0]]

>>> sort $ randomWalkPaths 2
[[0,-1,-2],[0,-1,-1],[0,-1,0],[0,0,-1],[0,0,0],[0,0,1],[0,1,0],[0,1,1],[0,1,2]]

=== __Hint__

The list monad can be a simple way to model non-deterministic computations.
For example, if we have one number that may be either 2 or 3,
and we want to multiply it by another number that may be either 5 or 6,
we can get the possible results as follows:

>>> :{
do
  x <- [2,3]
  y <- [5,6]
  return (x*y :: Int)
:}
[10,12,15,18]
-}
randomWalkPaths :: Int -> [[Int]]
randomWalkPaths = Solution.randomWalkPaths
