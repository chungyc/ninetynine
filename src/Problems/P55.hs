{- |
Description: `cbalTree`

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P55".
-}
module Problems.P55 (cbalTree) where

import           Problems.P54.Definitions
import qualified Solutions.P55            as Solution

-- | Construct completely balanced binary trees
--
-- In a completely balanced binary tree, the following property holds for every node:
-- The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal,
-- which means their difference is not greater than one.
--
-- Write a function 'cbalTree' to construct completely balanced binary trees for a given number of nodes.
-- The function should generate all solutions via backtracking.
-- Put the letter @\'x\'@ as information into all nodes of the tree.
cbalTree :: Int -> [Tree Char]
cbalTree = Solution.cbalTree
