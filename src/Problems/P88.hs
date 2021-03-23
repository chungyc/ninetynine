{- |
Description: 'connectedComponents'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P88".
-}
module Problems.P88 (connectedComponents) where

import           Problems.Graphs
import qualified Solutions.P88   as Solution

-- $setup
-- >>> import Data.List (sort)
-- >>> import Problems.P80

-- | Connected components.
--
-- Write a function that splits a graph into its connected components.
--
-- === Examples
--
-- >>> sort $ map sort $ connectedComponents $ toG $ Paths [[1,2,3,4,5], [2,4], [6,7]]
-- [[1,2,3,4,5],[6,7]]
connectedComponents :: G -> [[Vertex]]
connectedComponents = Solution.connectedComponents
