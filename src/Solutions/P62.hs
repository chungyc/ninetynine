{- |
Description: `atLevel`

Some solutions to "Problems.P62" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P62 (atLevel) where

import           Problems.BinaryTrees

-- | Collect the nodes at a given level in a list
--
-- A node of a binary tree is at level \(n\) if the path from the root to the node has length \(n-1\).
-- The root node is at level 1.
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch _ l r) n
  | n > 1     = atLevel l (n-1) ++ atLevel r (n-1)
  | otherwise = undefined
