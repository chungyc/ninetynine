{- |
Description: Construct completely balanced binary trees
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P55".
-}
module Problems.P55 (completelyBalancedTrees) where

import           Problems.BinaryTrees
import qualified Solutions.P55        as Solution

-- | In a completely balanced binary tree, the following property holds for every node:
-- The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal,
-- which means their difference is not greater than one.
--
-- Write a function to construct completely balanced binary trees for a given number of nodes.
-- The function should generate all solutions via backtracking.
--
-- === Examples
--
-- >>> printTreeList $ completelyBalancedTrees 4
-- [ Branch () (Branch () (Branch () Empty Empty) Empty) (Branch () Empty Empty)
-- , Branch () (Branch () Empty Empty) (Branch () (Branch () Empty Empty) Empty)
-- , Branch () (Branch () Empty Empty) (Branch () Empty (Branch () Empty Empty))
-- , Branch () (Branch () Empty (Branch () Empty Empty)) (Branch () Empty Empty)]
completelyBalancedTrees :: Int -> [Tree ()]
completelyBalancedTrees = Solution.completelyBalancedTrees
