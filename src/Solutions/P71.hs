{- |
Description: `internalPathLength`

Some solutions to "Problems.P71" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P71 (internalPathLength) where

import           Problems.MultiwayTrees

-- | Determine the internal path length of a tree.
--
-- We define the internal path length of a multiway tree as
-- the total sum of the path lengths from the root to all nodes of the tree.
-- By this definition, 'multitree5' has an internal path length of 9.
internalPathLength :: MultiwayTree a -> Int
internalPathLength = internalPathLength' 0

internalPathLength' :: Int -> MultiwayTree a -> Int
internalPathLength' l (MultiwayTree _ []) = l
internalPathLength' l (MultiwayTree _ ts) = (+) l $ sum $ map (internalPathLength' $ l+1) ts
