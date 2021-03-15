{- |
Description: 'completeBinaryTree'

Some solutions to "Problems.P63" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P63 (completeBinaryTree, isCompleteBinaryTree) where

import           Problems.BinaryTrees

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

=== __Hint__

We can assign an address number to each node in a complete binary tree
by enumerating the nodes in level-order, starting at the root with number 1.
For every node \(x\) with address \(a\), the following property holds:
The address of \(x\)'s left and right successors are \(2^a\) and \(2^a + 1\),
respectively, if they exist.  This fact can be used to elegantly construct
a complete binary tree structure.
-}
completeBinaryTree :: Int -> Tree ()
completeBinaryTree n = buildTree n 1

buildTree :: Int -> Int -> Tree ()
buildTree size index
  | index <= size = Branch () (buildTree size $ 2 * index) (buildTree size $ 2 * index + 1)
  | otherwise     = Empty

-- | Write a function to return whether a tree is a complete binary tree.
isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree t = isTree t (treeSize t) 1

isTree :: Tree a -> Int -> Int -> Bool
isTree Empty size index = index > size
isTree (Branch _ l r) size index = index <= size && isTree l size (2*index) && isTree r size (2*index+1)
