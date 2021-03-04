{- |
Description: 'spanningTrees'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P83".
-}
module Problems.P83 (spanningTrees, isTree, isConnected) where

import           Problems.Graphs
import qualified Solutions.P83   as Solution

-- | Construct all spanning trees
--
-- Mathematically, a tree is a graph with exactly one path between any two vertexes.
-- A spanning tree of a graph is a tree which includes all vertexes of the graph.
--
-- Write a function to construct all spanning trees of a given graph.
spanningTrees :: G -> [G]
spanningTrees = Solution.spanningTrees

-- | Write a function which returns whether a graph is a tree using 'spanningTrees'.
isTree :: G -> Bool
isTree = Solution.isTree

-- | Write a function which returns whether a graph is connected using 'spanningTrees'.
isConnected :: G -> Bool
isConnected = Solution.isConnected
