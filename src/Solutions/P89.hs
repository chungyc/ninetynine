{- |
Description: Bipartite graphs
Copyright: Copyright (C) 2023 Yoo Chung
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
  -- Went through all vertexes without encountering a contradiction with bipartiteness.
  | Set.null boundary && Set.null remaining = True
  -- Start again with disconnected component.
  | Set.null boundary = let (v', r') = Set.deleteFindMin remaining
                        in partition g (r', Set.singleton v', us, vs)
  -- If bipartite, then boundary' must necessarily end up the same set which will include vs.
  -- If any vertex in boundary' is also in us,
  -- then the set including us cannot be disjoint with that for vs.
  | not $ Set.disjoint boundary' us = False
  | otherwise = partition g (remaining', boundary', us', vs')
  where
    -- The neighbors of vertexes that are slated to be added to us.
    -- These neighbors will be added to the set which includes vs;
    -- don't revisit those already visited.
    boundary' = Set.difference (Set.unions $ Set.map (`neighbors` g) boundary) vs
    -- The remaining vertexes that are neither in us' or vs' yet.
    remaining' = Set.difference remaining boundary
    -- Switch roles between us and vs.
    us' = vs
    -- Add the boundary vertexes to the set including us; switch roles between us and vs.
    vs' = Set.union us boundary
