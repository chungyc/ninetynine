{- |
Description: `heightBalancedTrees`
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P59".
-}
module Problems.P59 (heightBalancedTrees) where

import           Problems.BinaryTrees
import qualified Solutions.P59        as Solution

-- | Construct height-balanced binary trees.
--
-- In a height-balanced binary tree, the following property holds for every node:
-- The height of its left subtree and the height of its right subtree are almost equal,
-- which means their difference is not greater than one.
--
-- Construct a list of all height-balanced binary trees with the given maximum height.
--
-- === Examples
--
-- >>> printTreeList $ heightBalancedTrees 2
-- [ Branch () (Branch () Empty Empty) Empty
-- , Branch () (Branch () Empty Empty) (Branch () Empty Empty)
-- , Branch () Empty (Branch () Empty Empty)]
--
-- >>> length $ heightBalancedTrees 4
-- 315
heightBalancedTrees :: Int -> [Tree ()]
heightBalancedTrees = Solution.heightBalancedTrees
