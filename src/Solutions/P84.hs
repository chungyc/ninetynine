{- |
Description: 'minimumSpanningTree'

Some solutions to "Problems.P84" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P84 (minimumSpanningTree) where

import           Data.Map.Lazy   (Map)
import qualified Data.Map.Lazy   as Map
import           Data.Maybe      (fromJust)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs

type WeightedG = (G, Map Edge Int)
type Partial = (Set Vertex, Set Edge, Map Int [Edge])

-- | Construct the minimum spanning tree.
--
-- Write a function which constructs the minimum spanning tree of a given weighted graph.
-- While the weight of an edge could be encoded in the graph represention itself,
-- here we will specify the weight of each edge in a separate map.
--
-- It is not defined what should happen if the given graph is not connected.
--
-- === __Hint__
--
-- The minimum spanning tree of a graph can be constructed
-- using [Prim's algorithm](https://en.wikipedia.org/wiki/Prim%27s_algorithm).
minimumSpanningTree :: G -> Map Edge Int -> G
minimumSpanningTree g@(G m) weights
  | null (vertexes g) = fromJust $ toGraph (Set.empty, Set.empty)
  | otherwise = fromJust $ toGraph (vs, es)
  where wg = (g, weights)
        (v, _) = Map.findMin m
        (vs, es, _) = expand wg (Set.singleton v, Set.empty, weightEdgesFromVertex v Set.empty wg)

expand :: WeightedG -> Partial -> Partial
expand wg r@(vs, _, boundary)
  | Map.null boundary = r
  | otherwise = incorporateMinEdge wg e r
  where e = extractMinEdge wg boundary vs

extractMinEdge :: WeightedG -> Map Int [Edge] -> Set Vertex -> Maybe (Edge, Map Int [Edge])
extractMinEdge wg boundary vs
  | Map.null boundary = Nothing
  | otherwise         = check e
  where (e, boundary') = extractFromBoundary boundary vs wg
        check (Edge (u', v'))
          | Set.member u' vs && Set.member v' vs = extractMinEdge wg boundary' vs
          | otherwise                            = Just (e, boundary')

extractFromBoundary :: Map Int [Edge] -> Set Vertex -> WeightedG -> (Edge, Map Int [Edge])
extractFromBoundary boundary vs wg = (e, boundary')
  where ((minWeight, es), boundary'') = Map.deleteFindMin boundary
        e = head es
        v = newVertex e vs
        boundary''' = reinsert (minWeight, tail es) boundary''
        boundary' = Map.unionWith (++) edgesFromVertex boundary'''
        edgesFromVertex = weightEdgesFromVertex v vs wg
        reinsert (_, []) m'  = m'
        reinsert (w, es') m' = Map.insert w es' m'

incorporateMinEdge :: WeightedG -> Maybe (Edge, Map Int [Edge]) -> Partial -> Partial
incorporateMinEdge _ Nothing r = r
incorporateMinEdge wg (Just (e@(Edge (u, v)), boundary')) (vs, es, _) = expand wg (vs', es', boundary')
  where es' = Set.insert e es
        vs' = Set.insert v $ Set.insert u vs

weightEdgesFromVertex :: Vertex -> Set Vertex -> WeightedG -> Map Int [Edge]
weightEdgesFromVertex v vs (G m, weights) = weightEdges (Set.map (\v' -> Edge (v,v')) $ Map.findWithDefault Set.empty v m) vs weights

weightEdges :: Set Edge -> Set Vertex -> Map Edge Int -> Map Int [Edge]
weightEdges es vs weights = Map.fromListWith (++) $ map (\e -> (Map.findWithDefault 0 e weights, [e])) $ filter (\(Edge (u,v)) -> not (Set.member u vs && Set.member v vs)) $ Set.toList es

newVertex :: Edge -> Set Vertex -> Vertex
newVertex (Edge (u, v)) vs
  | Set.member u vs = v
  | otherwise       = u
