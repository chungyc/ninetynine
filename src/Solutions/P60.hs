{- |
Description: `heightBalancedTreesWithNodes`
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P60" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P60 (heightBalancedTreesWithNodes) where

import           Problems.BinaryTrees
import           Problems.P59

-- | Construct all the height-balanced binary trees with a given number of nodes.
heightBalancedTreesWithNodes :: Int -> [Tree ()]
heightBalancedTreesWithNodes n = concat $ map treesWithHeight [minHeight n..maxHeight n]
  where treesWithHeight h = filter ((==) n . treeSize) $ heightBalancedTrees h

-- | Minimum number of nodes in a height-balanced tree of height @h@.
minNodes :: Int -> Int
minNodes 0 = 0
minNodes 1 = 1
minNodes h = 1 + minNodes (h-1) + minNodes (h-2)

minHeight :: Int -> Int
minHeight n = log2 (n+1)

log2 :: Int -> Int
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

-- | Maximum height for a height-balanced tree with @n@ nodes.
maxHeight :: Int -> Int
maxHeight n = maxHeight' n 0

maxHeight' :: Int -> Int -> Int
maxHeight' n h
  | minNodes h <= n && n < minNodes (h+1) = h
  | otherwise                              = maxHeight' n (h+1)
