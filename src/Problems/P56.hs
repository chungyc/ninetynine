{- |
Description: Symmetric binary trees
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P56".
-}
module Problems.P56 (symmetric) where

import           Problems.BinaryTrees
import qualified Solutions.P56        as Solution

-- $setup
-- >>> import Problems.BinaryTrees

-- | Let us call a binary tree symmetric if you can draw a vertical line through the root node
-- and then the right subtree is the mirror image of the left subtree.
-- Write a function 'symmetric' to check whether a given binary tree is symmetric.
-- We are only interested in the structure, not in the contents of the nodes.
--
-- === Examples
--
-- >>> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
-- False
--
-- >>> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
-- True
--
-- === __Hint__
--
-- Write a function @mirror@ first to check whether one tree is the mirror image of another.
symmetric :: Tree a -> Bool
symmetric = Solution.symmetric
