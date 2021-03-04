{- |
Description: 'spanningTrees'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P83".
-}
module Problems.P83 (spanningTrees, isTree, isConnected, graph83) where

import           Problems.Graphs
import           Problems.P80
import qualified Solutions.P83   as Solution

-- | Construct all spanning trees
--
-- Mathematically, a tree is a graph with exactly one path between any two vertexes.
-- A spanning tree of a graph is a tree which includes all vertexes of the graph.
--
-- Write a function to construct all spanning trees of a given graph.
--
-- You can use 'graph83' as an example graph to construct spanning trees from.
--
-- Examples:
--
-- >>> length $ spanningTrees graph83
-- 173
--
-- >>> (toG $ Paths [[1,2,4,5,6,7,8],[1,3],[5,10],[7,9]]) `elem` (spanningTrees graph83)
-- True
spanningTrees :: G -> [G]
spanningTrees = Solution.spanningTrees

-- | Write a function which returns whether a graph is a tree using 'spanningTrees'.
--
-- Examples:
--
-- >>> isTree graph83
-- False
--
-- >>> isTree $ toG $ Paths [[1,2,3],[1,4,5]]
-- True
isTree :: G -> Bool
isTree = Solution.isTree

-- | Write a function which returns whether a graph is connected using 'spanningTrees'.
--
-- Examples:
--
-- >>> isConnected graph83
-- True
--
-- >>> isConnected $ toG $ Lists ([1,2,3], [])
-- False
isConnected :: G -> Bool
isConnected = Solution.isConnected

-- | Graph for use as an example for 'spanningTrees'.
--
-- ![Graph with path 1,2,4,5,6,7,8, path 1,3,4,5, path 2,8, and path 5,10,7,9](images/Graphs/Example-P83.svg)
graph83 :: G
graph83 = toG $ Paths [[1,2,4,5,6,7,8],[1,3,4,6],[2,8],[5,10,7,9]]
