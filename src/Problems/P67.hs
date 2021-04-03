{- |
Description: A string representation of binary trees
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P67".
-}
module Problems.P67 (treeToString, stringToTree) where

import           Problems.BinaryTrees
import qualified Solutions.P67        as Solution

{- |
Somebody represents binary trees as strings of the following form:

> a(b(d,e),c(,f(g,)))

Write a function to generate this string representation from a binary tree.

=== Examples

>>> treeToString $ Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
"x(y,a(,b))"
-}
treeToString :: Tree Char -> String
treeToString = Solution.treeToString

{- |
Write a function to construct a tree from the string representation.

=== Examples

>>> stringToTree "x(y,a(,b))"
Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
-}
stringToTree :: String -> Tree Char
stringToTree = Solution.stringToTree
