module Problems.Graphs (
  Value, v1, v2, v3, v4, v5,
  List (List), lg,
  AdjacencyLists (AdjacencyLists), adjg,
  AdjacencyListMap (AdjacencyListMap), adjmg,
  Friendly (Friendly), fg,
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

lg :: List
lg = List ([1, 2, 3, 4, 5], [(1, 2), (1, 4), (2, 3), (2, 4), (3, 4), (4, 5)])

-- | Graph represented as with adjacency lists.
data AdjacencyLists = AdjacencyLists [(Int, [Int])]
  deriving (Eq, Show)

adjg :: AdjacencyLists
adjg = AdjacencyLists [(1, [2, 4]), (2, [1, 3, 4]), (3, [2, 4]), (4, [2, 3, 5]), (5, [4])]

-- | Adjacency lists in a map.
newtype AdjacencyListMap = AdjacencyListMap (Map.IntMap [Int])

adjmg :: AdjacencyListMap
adjmg = AdjacencyListMap $ Map.fromList l
  where AdjacencyLists l = adjg

-- | Human-friendly form.
--
-- Similar to DOT.
data Friendly = Friendly [[Int]]
  deriving (Eq, Show)

fg :: Friendly
fg = Friendly [[1, 2, 3, 4, 5], [1, 4], [2, 4]]

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
