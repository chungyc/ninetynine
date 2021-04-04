{- |
Description: Bipartite graphs
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P89" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P89 (bipartite) where

import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs

-- | Write a function that finds out whether a given graph is bipartite.
bipartite :: G -> Bool
bipartite g = partition g (vertexes g, Set.empty, Set.empty, Set.empty)

partition :: G -> (Set Vertex, Set Vertex, Set Vertex, Set Vertex) -> Bool
partition g (remaining, boundary, us, vs)
  | Set.null boundary && Set.null remaining = True
  | Set.null boundary = let (v', r') = Set.deleteFindMin remaining
                        in partition g (r', Set.singleton v', us, vs)
  | not $ Set.disjoint boundary' us = False
  | otherwise = partition g (remaining', boundary', us', vs')
  where boundary' = Set.difference (Set.unions $ Set.map (flip neighbors g) boundary) vs
        remaining' = Set.difference remaining boundary
        us' = vs
        vs' = Set.union us boundary
