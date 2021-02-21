{- |
Description: `symmetric`

Some solutions to "Problems.P56" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P56 (symmetric) where

import           Problems.P54.Definitions

-- | Symmetric binary trees.
--
-- Let us call a binary tree symmetric if you can draw a vertical line through the root node
-- and then the right subtree is the mirror image of the left subtree.
-- Write a function 'symmetric' to check whether a given binary tree is symmetric.
-- We are only interested in the structure, not in the contents of the nodes.
--
-- Hint: Write a function 'mirror' first to check whether one tree is the mirror image of another.
symmetric :: Tree a -> Bool
symmetric Empty                 = True
symmetric (Branch _ left right) = mirror left right

mirror :: Tree a -> Tree b -> Bool
mirror Empty Empty                     = True
mirror (Branch _ l r) (Branch _ l' r') = mirror l r' && mirror r l'
mirror _ _                             = False
