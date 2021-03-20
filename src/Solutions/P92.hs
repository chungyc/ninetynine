{- |
Description: 'gracefulTree'

Some solutions to "Problems.P92" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P92 (gracefulTree, gracefulTree') where

import           Data.List       (permutations, sort, sortOn)
import           Data.Map.Lazy   (Map, (!))
import qualified Data.Map.Lazy   as Map
import           Data.Maybe      (fromJust, isJust)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs
import           Problems.P81

{- |
Graceful tree labeling.

It has been conjectured that if graph \(G=(V,E)\) is a tree with \(n\) vertexes,
which necessarily means that it has \(n-1\) edges, then there is
a [/graceful labeling/](https://en.wikipedia.org/wiki/Graceful_labeling) of the tree.
This means that there is a way to label each vertex with integers from 1 to \(n\)
such that for every integer \(m\) from 1 to \(n-1\), there is an edge whose difference
in vertex labels is \(m\).  I.e., there is a bijection \(l : V \rightarrow \{ k \,|\, 1 \leq k \leq n \}\)
such that \(\{ |l(v)-l(v')| \,|\, \{v,v'\} \in E \} = \{ k \,|\, 1 \leq k \leq n-1 \}\).
There is no known counterexample, but neither is it proven that this is true for all trees.

Write a function which will gracefully label a tree graph.
If there is no graceful labeling for a particular tree,
return 'Nothing', and write up the counterexample for publication.

This implementation builds up a partial graceful labeling, adding one vertex at a time.
It gives up and tries another if a partial labeling cannot be extended.
-}
gracefulTree :: G -> Maybe (Map Vertex Int)
gracefulTree g
  | Set.null $ vertexes g = Just Map.empty
  | isTree g              = expand g vs Map.empty (Set.fromList [1..size]) Set.empty
  | otherwise             = undefined
  where vs = vertexesByDegree g
        size = length vs

-- | Trying vertexes with higher degrees first might constrain labeling more,
-- i.e, prune the search space more.
vertexesByDegree :: G -> [Vertex]
vertexesByDegree g = reverse $ sortOn (\v -> Set.size $ neighbors v g) $ Set.toList $ vertexes g

-- | Label one more vertex.
expand :: G               -- ^ Tree to label gracefully
       -> [Vertex]        -- ^ Vertexes remaining to be labeled
       -> Map Vertex Int  -- ^ Partial labeling of vertexes
       -> Set Int         -- ^ Set of labels yet to be used with vertexes
       -> Set Int         -- ^ Set of differences already determined for edges
       -> Maybe (Map Vertex Int)  -- ^ Graceful labeling of tree
expand _ [] lbls _ _              = Just lbls
expand g (v:vs) lbls vlbls ediffs = labelVertex g v vlbls vs lbls vlbls ediffs

-- | Try labeling a particular vertex with the given label candidates.
labelVertex :: G -> Vertex -> Set Int -> [Vertex] -> Map Vertex Int -> Set Int -> Set Int -> Maybe (Map Vertex Int)
labelVertex g v candidates vs lbls vlbls ediffs
  | Set.null candidates = Nothing
  | disjoint            = try $ expand g vs lbls' vlbls' ediffs'
  | otherwise           = next
  where candidate = Set.findMin candidates
        candidates' = Set.delete candidate candidates
        maybeDiffs = edgeDiffs g v candidate lbls
        diffs = fromJust maybeDiffs
        disjoint = isJust maybeDiffs && Set.disjoint diffs ediffs
        ediffs' = Set.union ediffs diffs
        lbls' = Map.insert v candidate lbls
        vlbls' = Set.delete candidate vlbls
        try Nothing = next
        try r       = r
        next = labelVertex g v candidates' vs lbls vlbls ediffs

-- | Gather the absolute differences between vertexes in the edge connected to the given vertex.
-- The given vertex would not have been labeled, so this cannot be double-counted.
edgeDiffs :: G -> Vertex -> Int -> Map Vertex Int -> Maybe (Set Int)
edgeDiffs g v lbl lbls
  | Set.size diffs == Set.size labeledNeighbors = Just diffs
  | otherwise                                   = Nothing
  where labeledNeighbors = Set.filter (\v' -> Map.member v' $ lbls) $ neighbors v g
        diffs = Set.map (\v' -> abs $ lbl - lbls ! v') labeledNeighbors

isTree :: G -> Bool
isTree g = all (\(v,v') -> length (paths v v' g) == 1) [ (v,v') | v <- vs, v' <- vs, v < v' ]
  where vs = Set.toList $ vertexes g

{- |
Graceful tree labeling.

It has been conjectured that if graph \(G=(V,E)\) is a tree with \(n\) vertexes,
which necessarily means that it has \(n-1\) edges, then there is
a [/graceful labeling/](https://en.wikipedia.org/wiki/Graceful_labeling) of the tree.
This means that there is a way to label each vertex with integers from 1 to \(n\)
such that for every integer \(m\) from 1 to \(n-1\), there is an edge whose difference
in vertex labels is \(m\).  I.e., there is a bijection \(l : V \rightarrow \{ k \,|\, 1 \leq k \leq n \}\)
such that \(\{ |l(v)-l(v')| \,|\, \{v,v'\} \in E \} = \{ k \,|\, 1 \leq k \leq n-1 \}\).
There is no known counterexample, but neither is it proven that this is true for all trees.

Write a function which will gracefully label a tree graph.
If there is no graceful labeling for a particular tree,
return 'Nothing', and write up the counterexample for publication.

This implementation tries all permutations of vertex labels
and checks if any are a graceful labeling.
-}
gracefulTree' :: G -> Maybe (Map Vertex Int)
gracefulTree' g
  | not $ null labelings = Just $ head labelings
  | otherwise            = Nothing
  where vs = Set.toList $ vertexes g
        candidates = map (Map.fromList . flip zip [1..]) $ permutations vs
        labelings = filter (isGracefulLabeling g) candidates

isGracefulLabeling :: G -> Map Vertex Int -> Bool
isGracefulLabeling g ls = diffs == lbls
  where diff (Edge (u,v)) = abs $ (ls ! u) - (ls ! v)
        diffs = sort $ map diff $ Set.toList $ edges g
        lbls = [1..(Set.size (vertexes g) - 1)]
