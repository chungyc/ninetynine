{- |
Description: `completeBinaryTree`
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P63".
-}
module Problems.P63 (completeBinaryTree, isCompleteBinaryTree) where

import           Problems.BinaryTrees
import qualified Solutions.P63        as Solution

{- |
Construct a complete binary tree.

A complete binary tree with height \(h\) is defined as follows:

* The levels 1, 2, 3, ..., \(h-1\) contain the maximum number of nodes,
  i.e., \(2^{i-1}\) at level \(i\).
* On level \(h\), which may contain less than the maximum possible number of nodes,
  all the nodes are "left-adjusted".  This means that in a level-order tree traversal
  all internal nodes come first, the leaves come second,
  and empty successors (the 'Empty's, which are not really nodes of the tree) come last.

Particularly, complete binary trees are used as data structures or addressing schemes for heaps.

Write a function to construct a complete binary tree with the given number of nodes.

=== Examples

>>> completeBinaryTree 4
Branch () (Branch () (Branch () Empty Empty) Empty) (Branch () Empty Empty)

=== __Hint__

We can assign an address number to each node in a complete binary tree
by enumerating the nodes in level-order, starting at the root with number 1.
For every node \(x\) with address \(a\), the following property holds:
The address of \(x\)'s left and right successors are \(2^a\) and \(2^a + 1\),
respectively, if they exist.  This fact can be used to elegantly construct
a complete binary tree structure.
-}
completeBinaryTree :: Int -> Tree ()
completeBinaryTree = Solution.completeBinaryTree

-- | Write a function to return whether a tree is a complete binary tree.
--
-- === Examples
--
-- >>> isCompleteBinaryTree $ Branch () (Branch () Empty Empty) Empty
-- True
--
-- >>> isCompleteBinaryTree $ Branch () Empty (Branch () Empty Empty)
-- False
isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree = Solution.isCompleteBinaryTree
