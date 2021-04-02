{- |
Description: 'ordersToTree'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P68" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P68 (preorder, inorder, ordersToTree) where

import           Problems.BinaryTrees

{- |
Return the in-order sequence of a binary tree.
-}
inorder :: Tree a -> [a]
inorder t = inorderGather t []

-- List will be in reverse order of elements added,
-- so traverse in reverse order.
inorderGather :: Tree a -> [a] -> [a]
inorderGather Empty xs          = xs
inorderGather (Branch x l r) xs = inorderGather l $ x : inorderGather r xs

{- |
Return the pre-order sequence of a binary tree.
-}
preorder :: Tree a -> [a]
preorder t = preorderGather t []

-- List will be in reverse order of elements added,
-- so traverse in reverse order.
preorderGather :: Tree a -> [a] -> [a]
preorderGather Empty xs          = xs
preorderGather (Branch x l r) xs = x : (preorderGather l $ preorderGather r xs)

{- |
Given the in-order and pre-order sequences of a binary tree, return the original binary tree.

The values in each node of the binary tree will be distinct,
in which case the tree is determined unambiguously.
-}
ordersToTree :: Eq a
             => [a]  -- ^ In-order sequence
             -> [a]  -- ^ Pre-order sequence
             -> Tree a  -- ^ Binary tree with the given in-order and pre-order sequences
ordersToTree [] [] = Empty
ordersToTree inseq (p:preseq) | p' == p   = Branch p l r
                              | otherwise = undefined
  where (inseqLeft, p':inseqRight) = break (p ==) inseq
        (preseqLeft, preseqRight) = splitAt (length inseqLeft) preseq
        l = ordersToTree inseqLeft preseqLeft
        r = ordersToTree inseqRight preseqRight
-- Failure to match patterns above or below indicate invalid sequences.
ordersToTree _ _ = undefined
