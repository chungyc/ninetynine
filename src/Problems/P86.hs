{- |
Description: Graph coloring
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P86".
-}
module Problems.P86 (colorGraph) where

import           Problems.Graphs
import qualified Solutions.P86   as Solution

-- $setup
-- >>> import Problems.P80

-- | Graph coloring assigns colors to each vertex in a way
-- such that no adjacent vertexes have the same color.
--
-- Write a function to color a graph
-- using [Welch-Powell's algorithm](https://graphstream-project.org/doc/Algorithms/Welsh-Powell/).
-- Use distinct integers to represent distinct colors,
-- and return the association list between vertexes and their colors.
--
-- === Examples
--
-- >>> colorGraph $ toG $ Paths [[1,2,3,4,5,10,8,6,9,7,2], [1,5], [1,6], [3,8], [4,9], [7,10]]
-- [(1,1),(2,2),(3,1),(4,2),(5,3),(6,2),(7,1),(8,3),(9,3),(10,2)]
--
-- ==== __Visualization with real colors__
--
-- ![Graph above represented with actual colors](images/Graphs/Example-P86.svg)
--
-- === __Hint__
--
-- Write a function to sort vertexes in decreasing order of degree, i.e., the number of neighbors.
-- The function 'Data.List.sortOn' may be useful for this.
colorGraph :: G -> [(Vertex, Int)]
colorGraph = Solution.colorGraph
