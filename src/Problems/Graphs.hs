module Problems.Graphs (
  Value, v1, v2, v3, v4, v5,
  List (List),
  AdjacencyLists (AdjacencyLists),
  AdjacencyListMap (AdjacencyListMap),
  Friendly (Friendly),
  Graph (vertexes, edges, neighbors, valid),
  ) where

import           Data.Map.Lazy

-- | A vertex, its contents, and its neighboring vertexes in a graph with direct adjacency lists.
--
-- Neighboring vertexes are included directly as values.
data Value a = Value a [Value a]
  deriving (Eq, Show)

v1 :: Value Int
v1 = Value 1 [v2, v4]

v2 :: Value Int
v2 = Value 2 [v1, v3, v3]

v3 :: Value Int
v3 = Value 3 [v2, v4]

v4 :: Value Int
v4 = Value 4 [v1, v2, v3, v5]

v5 :: Value Int
v5 = Value 5 [v4]

-- | Standard definition of graph.  Set of vertexes and set of edges.
data List a = List ([a], [(a, a)])
  deriving (Eq, Show)

-- | Graph represented as with adjacency lists.
data AdjacencyLists a = AdjacencyLists [(a, [a])]
  deriving (Eq, Show)

-- | Adjacency lists in a map.
data AdjacencyListMap a = AdjacencyListMap (Map a [a])
  deriving (Eq, Show)

-- | Human-friendly form.
--
-- Similar to DOT.
data Friendly a = Friendly [[a]]
  deriving (Eq, Show)

class Graph g where
  vertexes :: g -> [a]
  edges :: g -> [(a,a)]
  neighbors :: g -> a -> [a]
  valid :: g -> Bool

-- Labeled graphs. (Weighted graphs.)

-- Directed graphs.

-- Multigraphs.
