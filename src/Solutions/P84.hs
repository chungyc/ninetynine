{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

{- |
Description: Construct minimum spanning tree
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P84" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P84 (minimumSpanningTree) where

import           Data.Map.Lazy   (Map)
import qualified Data.Map.Lazy   as Map
import           Data.Maybe      (fromJust)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs

-- | Write a function which constructs the minimum spanning tree of a given weighted graph.
-- While the weight of an edge could be encoded in the graph represention itself,
-- here we will specify the weight of each edge in a separate map.
minimumSpanningTree :: G -> Map Edge Int -> G
minimumSpanningTree g@(G m) weights
  | null (vertexes g) = G Map.empty
  | otherwise = toSpanningTree vs es g
  where wg = (g, weights)
        (v, _) = Map.findMin m  -- chosen arbitrarily
        (vs, es, _) = expand wg (Set.singleton v, Set.empty, weightEdgesFromVertex wg v Set.empty)

type WeightedG = (G, Map Edge Int)

-- | Partially constructed minimum spanning tree.
type Partial = (Set Vertex,      -- Vertexes in the partially constructed tree.
                Set Edge,        -- Edges in the partially constructed tree.
                Map Int [Edge])  -- Boundary of edges between inside the tree and outside the tree, keyed by weight.

-- | Converts the given vertexes and edges into a graph only if they from a spanning tree of the given graph.
-- Otherwise, returns an empty graph.
toSpanningTree :: Set Vertex -> Set Edge -> G -> G
toSpanningTree vs es g
  | vs == vertexes g = fromJust $ toGraph (vs, es)
  | otherwise        = G Map.empty

-- | Expand the partially constructed minimum spanning tree by one edge.
expand :: WeightedG -> Partial -> Partial
expand wg r@(vs, _, boundary)
  | Map.null boundary = r
  | otherwise = incorporateMinEdge wg e r
  where e = extractMinEdge wg boundary vs

-- | Extract the edge with minimum weight which connects to outside the tree from the boundary.
--
-- While edges that do not connect to the outside of the tree are not added,
-- edges that between vertexes inside the tree can exist because they are not removed.
-- Such edges are skipped.
extractMinEdge :: WeightedG -> Map Int [Edge] -> Set Vertex -> Maybe (Edge, Map Int [Edge])
extractMinEdge wg boundary vs
  | Map.null boundary = Nothing
  | otherwise         = check e
  where (e, boundary') = extractFromBoundary wg boundary vs
        check (Edge (u', v'))
          | Set.member u' vs && Set.member v' vs = extractMinEdge wg boundary' vs
          | otherwise                            = Just (e, boundary')

-- | Extract the edge with minimum weight from the boundary.
--
-- The edge may not connect to a vertex outside the partially constructed tree.
extractFromBoundary :: WeightedG -> Map Int [Edge] -> Set Vertex -> (Edge, Map Int [Edge])
extractFromBoundary wg boundary vs = (e, boundary')
  where ((minWeight, es), boundary'') = Map.deleteFindMin boundary
        e = head es
        v = newVertex e vs
        boundary''' = reinsert (minWeight, tail es) boundary''
        boundary' = Map.unionWith (++) edgesFromVertex boundary'''
        edgesFromVertex = weightEdgesFromVertex wg v vs
        reinsert (_, []) m'  = m'
        reinsert (w, es') m' = Map.insert w es' m'

-- | Incorporate an edge into a partially constructed minimum spanning tree.
incorporateMinEdge :: WeightedG -> Maybe (Edge, Map Int [Edge]) -> Partial -> Partial
incorporateMinEdge _ Nothing r = r
incorporateMinEdge wg (Just (e@(Edge (u, v)), boundary')) (vs, es, _) = expand wg (vs', es', boundary')
  where es' = Set.insert e es
        vs' = Set.insert v $ Set.insert u vs

-- | Returns the weight to edges map for edges connected to a vertex.
--
-- Edges that do not connect to outside the partially constructed tree are excluded.
weightEdgesFromVertex :: WeightedG -> Vertex -> Set Vertex -> Map Int [Edge]
weightEdgesFromVertex (G m, weights) v vs =
  weightEdges (Set.map (\v' -> Edge (v,v')) $ Map.findWithDefault Set.empty v m) vs weights

-- | Returns the weight to edges map from the edges.
--
-- Edges that do not connect to outside the partially constructed tree are excluded.
weightEdges :: Set Edge -> Set Vertex -> Map Edge Int -> Map Int [Edge]
weightEdges es vs weights = Map.fromListWith (++) $ map weightEdge $ filter crosses $ Set.toList es
  where crosses (Edge (u, v)) = not $ Set.member u vs && Set.member v vs
        weightEdge e = (Map.findWithDefault 0 e weights, [e])

-- For the two vertexes in an edge, return the vertex outside the partially constructed tree.
newVertex :: Edge -> Set Vertex -> Vertex
newVertex (Edge (u, v)) vs
  | Set.member u vs = v
  | otherwise       = u
