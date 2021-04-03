{- |
Description: Cycles with a given vertex
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P82".
-}
module Problems.P82 (cycles) where

import           Problems.Graphs
import qualified Solutions.P82   as Solution

-- $setup
-- >>> import Data.List
-- >>> import Problems.P80

-- | A cycle is a path in a graph whose first and last vertexes are the same vertex.
-- No edges and no other vertexes repeat in the path.
--
-- Write a function which finds all cycles in the graph which include the given vertex.
--
-- === Examples
--
-- >>> sort $ cycles 1 $ toG $ Paths [[1,2,3], [1,3,4,2], [5,6]]
-- [[1,2,3],[1,2,4,3],[1,3,2],[1,3,4,2]]
cycles :: Vertex -> G -> [[Vertex]]
cycles = Solution.cycles
