{- |
Description: `dotstringToTree`

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P69".
-}
module Problems.P69 (dotstringToTree, treeToDotstring) where

import           Problems.BinaryTrees
import qualified Solutions.P69        as Solution

-- $setup
-- >>> import Problems.P54

{- |
Dotstring representation of binary trees.

Consider binary trees with nodes that are identified by single characters.
Such a tree can be represented by the preorder sequence of its nodes in which dots (@.@)
are inserted where an empty subtree is encountered during the tree traversal.

Write a function to convert a dotstring representation into its corresponding binary tree.

=== Examples

>>> dotstringToTree "xy..z0..."
Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty)
-}
dotstringToTree :: String -> Tree Char
dotstringToTree = Solution.dotstringToTree

{- |
Write a function to convert a binary tree to its dotstring representation.

=== Examples
>>> treeToDotstring tree1
"abd..e..c.fg..."
-}
treeToDotstring :: Tree Char -> String
treeToDotstring = Solution.treeToDotstring
