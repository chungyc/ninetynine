{- |
Description: 'minimumSpanningTree'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P84".
-}
module Problems.P84 (minimumSpanningTree) where

import           Data.Map.Lazy   (Map)
import           Problems.Graphs
import qualified Solutions.P84   as Solution

-- | Construct the minimum spanning tree.
--
-- Write a function which constructs the minimum spanning tree of a given weighted graph.
-- While the weight of an edge could be encoded in the graph represention itself,
-- here we will specify the weight of each edge in a separate map.
--
-- It is not defined what should happen if the given graph is not connected.
--
-- === __Hint__
--
-- The minimum spanning tree of a graph can be constructed
-- using [Prim's algorithm](https://en.wikipedia.org/wiki/Prim%27s_algorithm).
minimumSpanningTree :: G -> Map Edge Int -> G
minimumSpanningTree = Solution.minimumSpanningTree
