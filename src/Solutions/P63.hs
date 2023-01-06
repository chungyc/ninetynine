{- |
Description: Construct a complete binary tree
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P63" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P63 (completeBinaryTree, isCompleteBinaryTree) where

import           Problems.BinaryTrees

-- | Construct a complete binary tree with the given number of nodes.
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
