{- |
Description: `leaves`

Some solutions to "Problems.P61" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P61 (leaves) where

import           Problems.BinaryTrees

-- | Collect the leaves of a binary tree in a list.  A leaf is a node with no successors.
leaves :: Tree a -> [a]
leaves Empty                  = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r)         = leaves l ++ leaves r
