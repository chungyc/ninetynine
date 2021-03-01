{- |
Description: Binary trees

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P54".
-}
module Problems.P54 (Tree, leaf, tree1, tree2, tree3, tree4) where

import           Problems.BinaryTrees
import qualified Solutions.P54        as Solution

-- | Define a shorthand function for constructing a leaf node.
--
-- A leaf node in 'Tree' is a branch with two empty subtrees.
leaf :: a -> Tree a
leaf = Solution.leaf

-- | Define as the tree as shown in the following.
--
-- !['b' and 'c' are children of 'a', 'd' and 'e' are children of 'b', 'f' is the right child of 'c', 'g' is the left child of 'f'](images/BinaryTrees/tree1.svg)
tree1 :: Tree Char
tree1 = Solution.tree1

-- | Define as a binary tree consisting of only a single root node with value @\'a\'@.
tree2 :: Tree Char
tree2 = Solution.tree2

-- | Define as an empty binary tree.
tree3 :: Tree Char
tree3 = Solution.tree3

-- | Define as the following tree with integer values.
--
-- ![two nodes with both values of 2 are children of 1, 4 is the right child of the left 2](images/BinaryTrees/tree4.svg)
tree4 :: Tree Int
tree4 = Solution.tree4
