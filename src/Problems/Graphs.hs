module Problems.Graphs (
  Value, v1, v2, v3, v4, v5,
  List (List),
  AdjacencyLists (AdjacencyLists),
  AdjacencyListMap (AdjacencyListMap),
  Friendly (Friendly),
  Graph (vertexes, edges, neighbors, isValid),
  ) where

import qualified Data.IntMap.Strict as Map

-- | A vertex, its contents, and its neighboring vertexes in a graph with direct adjacency lists.
--
-- Neighboring vertexes are included directly as values.
data Value = Value Int [Value]
  deriving (Eq, Show)

v1 :: Value
v1 = Value 1 [v2, v4]

v2 :: Value
v2 = Value 2 [v1, v3, v3]

v3 :: Value
v3 = Value 3 [v2, v4]

v4 :: Value
v4 = Value 4 [v1, v2, v3, v5]

v5 :: Value
v5 = Value 5 [v4]

-- | Standard definition of graph.  Set of vertexes and set of edges.
data List = List ([Int], [(Int, Int)])
  deriving (Eq, Show)

-- | Graph represented as with adjacency lists.
data AdjacencyLists = AdjacencyLists [(Int, [Int])]
  deriving (Eq, Show)

-- | Adjacency lists in a map.
newtype AdjacencyListMap = AdjacencyListMap (Map.IntMap [Int])

-- | Human-friendly form.
--
-- Similar to DOT.
data Friendly a = Friendly [[Int]]
  deriving (Eq, Show)

class Graph g where
  vertexes :: g -> [Int]
  edges :: g -> [(Int,Int)]
  neighbors :: g -> Int -> [Int]
  isValid :: g -> Bool

instance Graph AdjacencyListMap where
  vertexes (AdjacencyListMap m) = Map.keys m
  edges (AdjacencyListMap m) = concat $ map (\(s, ds) -> [(s, d) | d <- ds]) (Map.toList m)
  neighbors (AdjacencyListMap m) v = Map.findWithDefault [] v m
  isValid (AdjacencyListMap m) = all (\ds -> all (flip Map.member $ m) ds) (Map.elems m)

-- Labeled graphs. (Weighted graphs.)

-- Directed graphs.

-- Multigraphs.
