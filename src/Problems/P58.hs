{- |
Description: `symmetricBalancedTrees`

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P58".
-}
module Problems.P58 (symmetricBalancedTrees) where

import           Problems.BinaryTrees
import qualified Solutions.P58        as Solution

-- | Construct all symmetric, completely balanced binary trees with a given number of nodes.
--
-- === Examples
--
-- >>> printTreeList $ symmetricBalancedTrees 5
-- [ Branch () (Branch () (Branch () Empty Empty) Empty) (Branch () Empty (Branch () Empty Empty))
-- , Branch () (Branch () Empty (Branch () Empty Empty)) (Branch () (Branch () Empty Empty) Empty)]
symmetricBalancedTrees :: Int -> [Tree ()]
symmetricBalancedTrees = Solution.symmetricBalancedTrees
