{- |
Description: Symmetric binary trees
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P56" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P56 (symmetric) where

import           Problems.BinaryTrees

-- | Check whether a given binary tree is symmetric.
symmetric :: Tree a -> Bool
symmetric Empty                 = True
symmetric (Branch _ left right) = mirror left right

mirror :: Tree a -> Tree b -> Bool
mirror Empty Empty                     = True
mirror (Branch _ l r) (Branch _ l' r') = mirror l r' && mirror r l'
mirror _ _                             = False
