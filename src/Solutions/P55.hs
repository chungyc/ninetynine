{- |
Description: `completelyBalancedTrees`

Some solutions to "Problems.P55" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P55 (completelyBalancedTrees) where

import           Problems.BinaryTrees

-- | Construct completely balanced binary trees
--
-- In a completely balanced binary tree, the following property holds for every node:
-- The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal,
-- which means their difference is not greater than one.
--
-- Write a function to construct completely balanced binary trees for a given number of nodes.
-- The function should generate all solutions via backtracking.
-- Put the letter @\'x\'@ as information into all nodes of the tree.
completelyBalancedTrees :: Int -> [Tree Char]
completelyBalancedTrees 0 = [Empty]
completelyBalancedTrees n
  | n `mod` 2 == 0 = generate (n `div` 2) ((n `div` 2) - 1) ++
                     generate ((n `div` 2) - 1) (n `div` 2)
  | otherwise      = generate (n `div` 2) (n `div` 2)

generate :: Int -> Int -> [Tree Char]
generate m n = [Branch 'x' l r | l <- ltrees, r <- rtrees]
  where ltrees = completelyBalancedTrees m
        rtrees = completelyBalancedTrees n
