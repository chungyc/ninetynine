{- |
Description: `ordersToTree`

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P68".
-}
module Problems.P68 (inorder, preorder, ordersToTree) where

import           Problems.BinaryTrees
import qualified Solutions.P68        as Solution

-- $setup
-- >>> import Problems.P54

{- |
Return the in-order sequence of a binary tree.

=== Examples

>>> inorder tree1
"dbeacgf"
-}
inorder :: Tree a -> [a]
inorder = Solution.inorder

{- |
Return the pre-order sequence of a binary tree.

=== Examples

>>> preorder tree1
"abdecfg"
-}
preorder :: Tree a -> [a]
preorder = Solution.preorder

{- |
Given the in-order and pre-order sequences of a binary tree, return the original binary tree.

The values in each node of the binary tree will be distinct,
in which case the tree is determined unambiguously.

=== Examples

>>> ordersToTree "dbeacgf" "abdecfg" == tree1
True
-}
ordersToTree :: Eq a
             => [a]  -- ^ In-order sequence
             -> [a]  -- ^ Pre-order sequence
             -> Tree a  -- ^ Binary tree with the given in-order and pre-order sequences
ordersToTree = Solution.ordersToTree
