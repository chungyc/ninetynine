module Problems.Graphs (
  Graph (vertexes, edges),
  ValueVertex (ValueVertex),
  VertexesEdges,
  AdjacencyLists,
  Paths,
  isGraph,
  isDirectedGraph,
  ) where

import qualified Data.Set as Set

-- | A graph is mathematically defined as a set of vertexes and a set of edges,
-- where an edge is a set of two elements from the set of vertexes.
-- I.e., if \(G = (V, E)\), where \(E \subseteq \{ \{v_1, v_2\} \,|\, v_1 \in V, v_2 \in V\}\),
-- then \(G\) is a graph.
--
-- The following is an example of a graph, with vertexes represented as circles and edges represented as lines.
--
-- ![Graph with vertexes 1, 2, 3, 4, 5, and edges {1, 2}, {1, 4}, {2, 3}, {2, 4}, {3, 4}, {4, 5}](images/Graphs/Example.svg)
--
-- While vertexes could be anything, we will use integers to identify vertexes in these problems.
--
-- === __Notes__
--
-- This introduction to graphs is substantially different from the one in
-- the original list of [Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems).
-- The original introduction would serve as an introduction to graphs in the context of Prolog,
-- but apparently was not updated to be more appropriate for other languages as the problems
-- were ported for Lisp and then Haskell.
--
-- This is a rewrite targeted to be more useful towards practicing Haskell.
-- Most of the graph problems themselves remain substantially the same.
class Graph g where
  -- | The set of vertexes.
  vertexes :: g -> [Int]

  -- | The set of edges.
  edges :: g -> [(Int, Int)]

-- | There are many ways to represent graphs in Haskell.
-- For example, the example graph can be represented by vertexes including their adjacent vertexes as values:
--
-- >>> :{
-- let v1 = ValueVertex 1 [v2, v4]
--     v2 = ValueVertex 2 [v1, v3, v3]
--     v3 = ValueVertex 3 [v2, v4]
--     v4 = ValueVertex 4 [v1, v2, v3, v5]
--     v5 = ValueVertex 5 [v4]
-- :}
--
-- === __Tying the knot__
--
-- While many languages can do something similar with objects pointing or referencing each other,
-- most of them cannot do so by /value/ if there are any cycles.
-- This is possible in Haskell thanks to lazy evaluation,
-- and this technique is called ["tying the knot"](https://wiki.haskell.org/Tying_the_Knot).
--
-- However, tying the knot to represent cycles may not be useful in many situations.
-- It is equivalent to and indistinguishable from an infinite multiway tree.
-- This can be resolved by assuming that vertexes with the same label are the same vertex.
-- Unfortunately, this allows for an inconsistent graph representation,
-- and there is no general way to confirm that a graph representation is consistent.
--
-- For example, there are no graphs consistent with the following representation:
--
-- >>> :{
-- let v1  = ValueVertex 1 [v2]
--     v2  = ValueVertex 2 [v3]
--     v3  = ValueVertex 3 [v1']
--     v1' = ValueVertex 1 [v3]
-- :}
--
-- On the other hand, the following is a consistent representation of a graph.
-- Unforunately, it cannot be proven that it is consistent with just the values.
-- For the general case, even knowledge of the code will not always be enough;
-- otherwise, this would lead to an algorithm for the [halting problem](https://brilliant.org/wiki/halting-problem/).
--
-- >>> :{
-- let v1 = ValueVertex 1 [v2]
--     v2 = ValueVertex 2 [v1]
-- :}
--
-- If there are no cycles in the graph, this is not an issue.
-- In fact, trees are graphs which are often represented this way.
data ValueVertex = ValueVertex Int [ValueVertex]
  deriving (Eq, Show)

-- | Graphs can also be represented by the set of its vertexes and the set of its edges.
-- This is close to the standard mathematical definition of a graph.
--
-- For example, the example graph can be represented as:
--
-- >>> ([1, 2, 3, 4, 5], [(1, 2), (1, 4), (2, 3), (2, 4), (3, 4), (4, 5)])
-- ...
type VertexesEdges = ([Int], [(Int, Int)])

-- | A common approach to representing graphs are /adjacency lists/.
-- For each vertex, it is associated with the vertexes that are adjacent to it.
--
-- For example, the example graph can be represented as:
--
-- >>> [(1, [2, 4]), (2, [1, 3, 4]), (3, [2, 4]), (4, [1, 2, 3, 5]), (5, [4])]
-- ...
type AdjacencyLists = [(Int, [Int])]

-- | The previous approaches can be verbose and error-prone for humans to use.
--
-- An easier way for humans is to use a sequence of vertexes to represent both
-- the vertexes and edges.  Within a sequence is implicitly
-- an edge between consecutive vertexes in the sequence.
-- E.g., a sequence @[a, b, c, ...]@ means there are vertexes @a@, @b@, @c@, ... and edges @(a, b)@, @(b, c)@, ...
-- There will be as many sequences as required to represent all edges in the graph.
--
-- For example, the example graph can be represented as:
--
-- >>> [[1, 2, 3, 4, 5], [1, 4], [2, 4]]
-- ...
--
-- === __DOT graphs__
--
-- This is similar to the approach used by very basic DOT graphs.
-- which are commonly used to generate [visualizations of graphs](https://graphviz.org/).
type Paths = [[Int]]

-- | The definition of a graph as explained here is that of an /undirected/ graph,
-- where there is no direction to the edges.  If we store edges as an ordered pair of
-- vertexes, then this means that if \((u, v)\) is an edge, then \((v, u)\) must
-- also be an edge.
--
-- A value being of a particular type may not be enough to ensure that it represents
-- an undirected graph.  For example, an edge in 'VertexesEdges' may have an element
-- which is not a vertex of the graph.  'isGraph' is a function which will check
-- whether a 'Graph' value is a valid representation of an undirected graph.
-- Here, we will arbitrarily require that if edges are stored as ordered pairs,
-- then for an undirected graph @g@, if @(u, v)@ is in @'edges' g@, then  @(v, u)@
-- must also be in @'edges' g@.
isGraph :: Graph g => g -> Bool
isGraph g = edgesHaveVertexes && edgesAreNotDirectional
  where vs = Set.fromList $ vertexes g
        es = Set.fromList $ edges g
        -- Vertexes in edges must be vertexes.
        edgesHaveVertexes = Set.foldl (\r -> \(u, v) -> r && Set.member u vs && Set.member v vs) True es
        -- Edges have no direction; i.e., (u, v) and (v, u) are the same edge.
        -- We will require that they must both be in the edges list for undirected graphs.
        --
        -- Requiring that at most one of (u, v) or (v, u) is in the edges list
        -- is a reasonable alternative requirement, but we will not use this one here.
        -- It would make it infeasible to check the validity of a graph with
        -- some representations using only the 'Graph' type class.
        -- In particular, we would not be able to check whether a value of 'VertexesEdges'
        -- or 'AdjacencyLists' would be a valid undirected graph without incorporating details
        -- outside the 'Graph' type class.
        edgesAreNotDirectional = Set.foldl (\r -> \(u, v) -> r && not (Set.member (v, u) es)) True es

-- | Edges may not only be stored as ordered pairs, but they may also be /defined/ as ordered pairs.
-- I.e., \((u, v)\) would not be the same edge as \((v, u)\).
-- Such graphs are called /directed/ graphs.
--
-- The graph representations in this module are able to represent directed graphs as well.
-- 'isDirectedGraph'  is a function which will check whether a 'Graph' value is a valid
-- representation of a directed graph.  The difference from 'isGraph' is that it does
-- not require the same symmetry condition for the vertexes in an edge.
isDirectedGraph :: Graph g => g -> Bool
isDirectedGraph g = edgesHaveVertexes
  where vs = Set.fromList $ vertexes g
        es = Set.fromList $ edges g
        -- Vertexes in edges must be vertexes.
        edgesHaveVertexes = Set.foldl (\r -> \(u, v) -> r && Set.member u vs && Set.member v vs) True es
