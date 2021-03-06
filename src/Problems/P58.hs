{- |
Description: `symCbalTrees`

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P58".
-}
module Problems.P58 (symCbalTrees) where

import           Problems.BinaryTrees
import qualified Solutions.P58        as Solution

-- | Construct all symmetric, completely balanced binary trees with a given number of nodes.
--
-- === Examples
--
-- >>> printTreeList $ symCbalTrees 5
-- [ Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))
-- , Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty)]
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = Solution.symCbalTrees
