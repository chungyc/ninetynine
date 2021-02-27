module Problems.Graphs (
  Graph (vertexes, edges),
  Var,
  Lists (Lists),
  Adjacency (Adjacency),
  Paths (Paths),
  G (G),
  Vertex,
  Edge (Edge),
  sets,
  toGraph,
  ) where

import           Data.List     (permutations)
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Maybe    (fromJust)
import           Data.Set      (Set)
import qualified Data.Set      as Set

-- $setup
-- >>> import Problems.Graphs.Arbitrary ()

-- | A graph is mathematically defined as a set of vertexes and a set of edges,
-- where an edge is a set of two elements from the set of vertexes.
-- I.e., if \(G = (V, E)\), where \(E \subseteq \{ \{v_1, v_2\} \,|\, v_1 \in V, v_2 \in V\}\),
-- then \(G\) is a graph.
--
-- The following is an example of a graph, with vertexes represented as circles and edges represented as lines.
--
-- ![Graph with vertexes 1, 2, 3, 4, 5, and edges {1, 2}, {1, 4}, {2, 3}, {2, 4}, {3, 4}, {4, 5}](images/Graphs/Example.svg)
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
  vertexes :: g -> Set Vertex

  -- | The set of edges.
  edges :: g -> Set Edge

  -- | The sets of vertexes and edges for a graph.  I.e.,
  --
  -- prop> sets (g :: G) == (vertexes g, edges g)
  sets :: g -> (Set Vertex, Set Edge)
  sets g = (vertexes g, edges g)

  -- | Build a graph of type @g@, given a set of vertexes and a set of edges.
  --
  -- If the sets are not consistent with a valid graph, return 'Nothing'.
  --
  -- prop> toGraph (sets g) == (g :: G)
  toGraph :: (Set Vertex, Set Edge) -> Maybe g
  toGraph = undefined

isValidGraph :: (Set Vertex, Set Edge) -> Bool
isValidGraph (vs, es) = Set.isSubsetOf vs' vs
  where vs' = Set.foldl (\s -> \(Edge (u, v)) -> Set.insert u $ Set.insert v s) Set.empty es

-- | A vertex in a graph.
--
-- In general, vertexes can be anything.  For these problems, vertexes will be integers.
type Vertex = Int

-- | An edge in a graph.
--
-- We will only deal with /undirected/ graphs.  I.e.,
--
-- prop> Edge (u, v) == Edge (v, u)
data Edge = Edge (Vertex, Vertex)
  deriving Show

-- | Edges in undirected graphs have no direction, so the order of the vertexes do not matter.
instance Eq Edge where
  (==) (Edge e) (Edge e') = normalize e == normalize e'

-- | We define an order for the sole purpose of making output reproducible.
-- The ordering has no meaning otherwise.
instance Ord Edge where
  compare (Edge e) (Edge e') = compare (normalize e) (normalize e')

-- | Normalizes the representation of an edge to a single representation.
--
-- I.e., @normalize (u, v) == normalize (v, u)@.
normalize :: (Vertex, Vertex) -> (Vertex, Vertex)
normalize e@(u, v)
  | u <= v    = e
  | otherwise = (v, u)

-- | A default implementation for comparing graph equality.
equals :: Graph g => g -> g -> Bool
equals g g' = vs == vs' && es == es'
  where (vs, vs') = (vertexes g, vertexes g')
        (es, es') = (edges g, edges g')

-- | There are many ways to represent graphs in Haskell.
-- For example, the example graph can be represented by variables including their adjacent vertexes as values:
--
-- >>> :{
-- let v1 = Var 1 [v2, v4]
--     v2 = Var 2 [v1, v3, v3]
--     v3 = Var 3 [v2, v4]
--     v4 = Var 4 [v1, v2, v3, v5]
--     v5 = Var 5 [v4]
-- :}
--
-- We will not be using this representation of graphs in the problems.
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
-- let v1  = Var 1 [v2]
--     v2  = Var 2 [v3]
--     v3  = Var 3 [v1']
--     v1' = Var 1 [v3]
-- :}
--
-- On the other hand, the following is a consistent representation of a graph.
-- Unforunately, it cannot be proven that it is consistent with just the values.
-- For the general case, even knowledge of the code will not always be enough;
-- otherwise, this would lead to an algorithm for the [halting problem](https://brilliant.org/wiki/halting-problem/).
--
-- >>> :{
-- let v1 = Var 1 [v2]
--     v2 = Var 2 [v1]
-- :}
--
-- If there are no cycles in the graph, this is not an issue.
-- In fact, trees are graphs which are often represented this way.
data Var = Var Vertex [Var]
  deriving Show

instance Eq Var where
  (==) (Var v vs) (Var v' vs')
    | v /= v'   = False
    | otherwise = any (vs ==) (permutations vs')

-- | Graphs can also be represented by the lists of its vertexes and edges.
-- This is close to the standard mathematical definition of a graph.
--
-- For example, the example graph can be represented as:
--
-- >>> Lists [1, 2, 3, 4, 5] [(1, 2), (1, 4), (2, 3), (2, 4), (3, 4), (4, 5)]
-- ...
data Lists = Lists [Vertex] [(Vertex, Vertex)]
  deriving Show

instance Graph Lists where
  vertexes (Lists vs _) = Set.fromList vs
  edges (Lists _ es) = Set.fromList $ map Edge es

instance Eq Lists where
  (==) g g' = equals g g'

-- | A common approach to representing graphs are with /adjacency lists/.
-- As the name implies, for each vertex it lists its adjacent vertexes
--
-- For example, the example graph can be represented as:
--
-- >>> Adjacency [(1, [2, 4]), (2, [1, 3, 4]), (3, [2, 4]), (4, [1, 2, 3, 5]), (5, [4])]
-- ...
data Adjacency = Adjacency [(Vertex, [Vertex])]
  deriving Show

instance Graph Adjacency where
  vertexes (Adjacency vs) = Set.fromList $ map fst vs
  edges (Adjacency vs) = Set.fromList $ concat $ map (\(v, es) -> [Edge (v, e) | e <- es]) vs

instance Eq Adjacency where
  (==) g g' = equals g g'

-- | The previous approaches can be verbose and error-prone for humans to use.
--
-- An easier way for humans is to use paths of vertexes to represent both the vertexes and edges.
-- Within a path is implicitly an edge between consecutive vertexes.
-- E.g., a path @[a, b, c, ...]@ means there are vertexes @a@, @b@, @c@, ... and edges @(a, b)@, @(b, c)@, ...
-- There will be as many paths as required to represent all edges in the graph.
--
-- For example, the example graph can be represented as:
--
-- >>> Paths [[1, 2, 3, 4, 5], [1, 4], [2, 4]]
-- ...
--
-- === __DOT graphs__
--
-- This is similar to the approach used by DOT graphs,
-- which are commonly used to generate [visualizations of graphs](https://graphviz.org/).
data Paths = Paths [[Vertex]]
  deriving Show

instance Graph Paths where
  vertexes (Paths ps) = Set.fromList $ concat ps

  edges (Paths ps) = Set.fromList $ concat $ map toEdges ps
    where toEdges []             = []
          toEdges [_]            = []
          toEdges (u : vs@(v:_)) = Edge (u, v) : toEdges vs

  toGraph g
    | isValidGraph g = Just $ Paths $ snd $ extractPaths (fromJust $ toGraph g :: G, [])
    | otherwise      = Nothing

extractPaths :: (G, [[Vertex]]) -> (G, [[Vertex]])
extractPaths e@(g@(G m), ps)
  | Map.null m = e
  | otherwise   = extractPaths (g', p' : ps)
  where (g', p') = extractPathFrom (pathStart g) g

pathStart :: G -> Vertex
pathStart (G m) = fst $ head $ candidates $ Map.toList roots
  -- Try to choose a vertex in zero or one edge.
  -- Make it more likely to get paths such as [[1,2,3]] instead of [[2,3],[1,2]].
  where roots = Map.filter ((>=) 1 . Set.size) m
        candidates [] = Map.toList m
        candidates vs = vs

extractPathFrom :: Vertex -> G -> (G, [Vertex])
extractPathFrom v g = extractPath v (g, [])

extractPath :: Vertex -> (G, [Vertex]) -> (G, [Vertex])
extractPath v (g@(G m), p)
  | Set.null neighbors = (g, v : p)
  | otherwise          = extractPath v' (deleteEdge v v' g, v : p)
  where neighbors = Map.findWithDefault Set.empty v m
        v'        = Set.findMin neighbors

deleteEdge :: Vertex -> Vertex -> G -> G
deleteEdge u v (G m) = G $ delete u v $ delete v u m
  where delete u' v' m' = Map.update (toMaybe . Set.delete v') u' m'
        toMaybe vs
          | Set.null vs   = Nothing
          | otherwise = Just vs

instance Eq Paths where
  (==) g g' = equals g g'

-- | Represents a graph with a map where a vertex is a key and the set of its neighbors is the value.
--
-- This is basically an indexed version of adjacency lists.
-- This representation may be the easiest for graph functions to use,
-- and we will use it as the default representation of graphs.
--
-- Example:
--
-- >>> import qualified Data.Map as M
-- >>> import qualified Data.Set as S
-- >>> G $ M.map S.fromList $ M.fromList [(1, [2, 4]), (2, [1, 3, 4]), (3, [2, 4]), (4, [1, 2, 3, 5]), (5, [4])]
-- ...
data G = G (Map Vertex (Set Vertex))
  deriving (Eq, Show)

instance Graph G where
  vertexes (G m) = Map.keysSet m

  edges (G m) = Map.foldlWithKey addVertex Set.empty m
    where addVertex s v vs = Set.union s $ toEdges v vs
          toEdges v vs = Set.map (\u -> Edge (v, u)) vs

  toGraph (vs, es)
    | Set.isSubsetOf vs' vs = Just $ G $ Set.foldl insertEdge Map.empty es
    | otherwise = Nothing
    where vs' = Set.foldl (\s -> \(Edge (u, v)) -> Set.insert u $ Set.insert v s) Set.empty es
          insertEdge m (Edge (u, v)) = insertNeighbor u v $ insertNeighbor v u m
          insertNeighbor u v m = Map.insertWith Set.union u (Set.singleton v) m
