{- |
Description: `internalPathLength`

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P71".
-}
module Problems.P71 (internalPathLength) where

import           Problems.MultiwayTrees
import qualified Solutions.P71          as Solution

-- | Determine the internal path length of a tree.
--
-- We define the internal path length of a multiway tree as
-- the total sum of the path lengths from the root to all nodes of the tree.
-- By this definition, 'multitree5' has an internal path length of 9.
--
-- === Examples
--
-- >>> internalPathLength multitree5
-- 9
--
-- >>> internalPathLength multitree4
-- 2
internalPathLength :: MultiwayTree a -> Int
internalPathLength = Solution.internalPathLength
