{- |
Description: 'cycles'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P82".
-}
module Problems.P82 (cycles) where

import           Problems.Graphs
import qualified Solutions.P82   as Solution

-- | Cycles with a given node.
--
-- A cycle is a path in a graph whose first and last vertexes are the same vertex.
-- No other vertexes repeat in the path.
--
-- Write a function which finds all cycles in the graph which include the given vertex.
--
-- Example:
--
-- >>> sort $ cycles 1 $ toG $ Paths [[1,2,3], [1,3,4,2], [5,6]]
-- [[1,2,3,4],[1,2,4],[1,3,2,4],[1,3,4]]
cycles :: Vertex -> G -> [[Vertex]]
cycles = Solution.cycles
