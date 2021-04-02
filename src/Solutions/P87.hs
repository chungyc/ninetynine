{- |
Description: 'depthFirst'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P87" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P87 (depthFirst) where

import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs

-- | Depth-first order graph traversal.
--
-- Write a predicate that generates a depth-first order graph traversal sequence.
-- The starting point should be specified,
-- and the output should be a list of nodes that are reachable from this starting point in depth-first order.
depthFirst :: G -> Vertex -> [Vertex]
depthFirst g v = reverse $ snd $ traverseVertex g v (Set.empty, [])

traverseVertex :: G -> Vertex -> (Set Vertex, [Vertex]) -> (Set Vertex, [Vertex])
traverseVertex g v (visited, vs)
  | Set.member v visited = (visited, vs)
  | otherwise            = traverseNeighbors g ns (visited', vs')
  where ns = Set.toList $ neighbors v g
        visited' = Set.insert v visited
        vs' = v : vs

traverseNeighbors :: G -> [Vertex] -> (Set Vertex, [Vertex]) -> (Set Vertex, [Vertex])
traverseNeighbors _ [] r = r
traverseNeighbors g (v:vs) r = traverseNeighbors g vs r'
  where r' = traverseVertex g v r
