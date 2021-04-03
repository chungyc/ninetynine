{- |
Description: Binary tree layout; constant distance at each level
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P65" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P65 (layoutLevelConstant) where

import           Problems.BinaryTrees

{- |
An alternative layout method is depicted in the illustration below:

![Layout example](images/BinaryTrees/Layout-P65.svg)

Find out the rules and write the corresponding function.
Use the same conventions as in "Problems.P64".
-}
layoutLevelConstant :: Tree a -> Tree (a, (Int,Int))
layoutLevelConstant t = shift amount layoutTree
  where layoutTree = layout t 0 1 distance
        distance = 2^(treeHeight t - 2)
        amount = 1 - minPos layoutTree

layout :: Tree a -> Int -> Int -> Int -> Tree (a, (Int, Int))
layout Empty _ _ _ = Empty
layout (Branch x l r) pos depth distance = Branch (x, (pos, depth)) l' r'
  where l' = layout l (pos - distance) depth' distance'
        r' = layout r (pos + distance) depth' distance'
        depth' = depth + 1
        distance' = distance `div` 2

shift :: Int -> Tree (a, (Int, Int)) -> Tree (a, (Int, Int))
shift _ Empty = Empty
shift amount (Branch (v, (x,y)) l r) = Branch (v, (x+amount,y)) l' r'
  where l' = shift amount l
        r' = shift amount r

minPos :: Tree (a, (Int, Int)) -> Int
minPos Empty                    = 0
minPos (Branch (_, (x, _)) l _) = min x $ minPos l
-- No need to check right subtree.
-- Halving horizontal distance at each level means no node in the right subtree
-- can ever be positioned to the left of the parent node.
