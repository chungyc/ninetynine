{- |
Description: Construct minimum spanning tree
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P84".
-}
module Problems.P84 (minimumSpanningTree, weights84) where

import           Data.Map.Lazy   (Map)
import qualified Data.Map.Lazy   as Map
import           Problems.Graphs
import qualified Solutions.P84   as Solution

-- $setup
-- >>> import Data.Map ((!))
-- >>> import qualified Data.Set as Set
-- >>> import Problems.Graphs
-- >>> import Problems.P83

-- | Write a function which constructs the minimum spanning tree of a given weighted graph.
-- While the weight of an edge could be encoded in the graph represention itself,
-- here we will specify the weight of each edge in a separate map.
--
-- === Examples
--
-- >>> let t = minimumSpanningTree graph83 weights84
-- >>> Set.foldr (\e w -> w + weights84 ! e) 0 $ edges t
-- 33
--
-- === __Hint__
--
-- The minimum spanning tree of a graph can be constructed
-- using [Prim's algorithm](https://en.wikipedia.org/wiki/Prim%27s_algorithm)
-- or [Kruskal's algorithm](https://en.wikipedia.org/wiki/Kruskal%27s_algorithm).
minimumSpanningTree :: G -> Map Edge Int -> G
minimumSpanningTree = Solution.minimumSpanningTree

-- | Edge to weight map for use with 'Problem.graph83' as an example for 'minimumSpanningTree'.
weights84 :: Map Edge Int
weights84 = Map.fromList [ (Edge (1, 2), 4)
                         , (Edge (1, 3), 1)
                         , (Edge (1, 5), 2)
                         , (Edge (2, 4), 8)
                         , (Edge (2, 8), 6)
                         , (Edge (3, 4), 10)
                         , (Edge (4, 5), 5)
                         , (Edge (4, 6), 5)
                         , (Edge (5, 6), 3)
                         , (Edge (5, 10), 4)
                         , (Edge (6, 7), 1)
                         , (Edge (7, 8), 2)
                         , (Edge (7, 9), 7)
                         , (Edge (7, 10), 11)]
