{- |
Description: Depth-first graph traversal
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P87".
-}
module Problems.P87 (depthFirst) where

import           Problems.Graphs
import qualified Solutions.P87   as Solution

-- $setup
-- >>> import Problems.Graphs
-- >>> import Problems.P80

-- | Write a function that generates a depth-first order graph traversal sequence.
-- The starting point should be specified,
-- and the output should be a list of nodes that are reachable from this starting point in depth-first order.
--
-- === Examples
--
-- >>> let xs = depthFirst (toG $ Paths [[1,2,3,4,5], [2,4], [6,7]]) 1
-- >>> xs `elem` [[1,2,3,4,5], [1,2,4,5,3], [1,2,4,3,5]]
-- True
depthFirst :: G -> Vertex -> [Vertex]
depthFirst = Solution.depthFirst
