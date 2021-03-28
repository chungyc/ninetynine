{- |
Description: 'regularGraphs'

Some solutions to "Problems.P94" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P94 (regularGraphs) where

import           Data.List       (nubBy)
import qualified Data.Map.Lazy   as Map
import           Data.Maybe      (fromJust)
import qualified Data.Set        as Set
import           Problems.Graphs
import           Problems.P26
import           Problems.P85

{- |
Generate \(k\)-regular graphs with \(n\) vertexes.

In a \(k\)-regular graph, all vertexes have a degree of \(k\).
I.e., the number of edges incident in each vertex is \(k\).
How many non-isomorphic 3-regular graphs with 6 vertexes are there?
-}
regularGraphs :: Int  -- ^ \(n\)
              -> Int  -- ^ \(k\)
              -> [G]  -- ^ non-isomorphic \(k\)-regular graphs with \(n\) vertexes
regularGraphs n k = nubBy isomorphic $ buildGraphs k [1..n] emptyGraph
  where emptyGraph = fromJust $ toGraph (Set.fromList [1..n], Set.empty)

-- | Build up the regular graph by adding edges for each vertex one by one.
-- An earlier vertex would attempt all possibilities that add an edge
-- to a later vertex, so there is no need to add edges to earlier vertexes,
-- and neither is there a need to try different orders of vertexes.
buildGraphs :: Int -> [Vertex] -> G -> [G]
buildGraphs _ [] g = [g]
buildGraphs k (v:vs) g = concat $ map (buildGraphs k vs) gs
  where gs = expand k g v vs

-- | Expand the graph to all possible graphs that have edges added
-- from the given vertex to a combination of the remaining vertexes.
expand :: Int -> G -> Vertex -> [Vertex] -> [G]
expand k g v vs = map (addEdges g v) candidates
  where k' = k - Set.size (neighbors v g)
        candidates = filter (all $ \v' -> Set.size (neighbors v' g) < k) $ combinations k' vs

addEdges :: G -> Vertex -> [Vertex] -> G
addEdges g v vs = foldl (addEdge v) g vs

addEdge :: Vertex -> G -> Vertex -> G
addEdge v (G g) v' = G $ Map.adjust (Set.insert v) v' $ Map.adjust (Set.insert v') v g
