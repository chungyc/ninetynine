module Problems.Graphs (
  ValueVertex (ValueVertex),
  VertexesEdges,
  AdjacencyLists,
  Friendly,
  ) where

-- | A graph is mathematically defined as a set of vertexes and a set of edges,
-- where an edge is a set of two elements from the set of vertexes.
-- I.e., if \(G = (V, E)\), where \(E \subseteq \{ \{v_1, v_2\} \,|\, v_1 \in V, v_2 \in V, v_1 \neq v_2\}\),
-- then \(G\) is a graph.  The following is an example of a graph, with vertexes represented as circles and edges represented as lines.
--
-- ![Graph with vertexes 1, 2, 3, 4, 5, and edges {1, 2}, {1, 4}, {2, 3}, {2, 4}, {3, 4}, {4, 5}](images/Graphs/Example.svg)
--
-- There are many ways to represent graphs in Haskell.
-- For example, the graph above can be represented as:
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
-- For example, this is a representation inconsistent with any graph:
--
-- >>> :{
-- let v1  = ValueVertex 1 [v2]
--     v2  = ValueVertex 2 [v3]
--     v3  = ValueVertex 3 [v1']
--     v1' = ValueVertex 1 [v3]
-- :}
--
-- On the other hand, this is a consistent representation of a graph.
-- However, it cannot be proven that it is consistent with just the values.
-- For the general case, even knowledge of the code will not always be enough;
-- otherwise, this would lead to an algorithm for the [halting problem](https://brilliant.org/wiki/halting-problem/).
--
-- >>> :{
-- let v1 = ValueVertex 1 [v2]
--     v2 = ValueVertex 2 [v1]
-- :}
--
-- If there no cycles in the graph, this is not an issue.
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
type Friendly = [[Int]]

-- Labeled graphs. (Weighted graphs.)

-- Directed graphs.

-- Multigraphs.
