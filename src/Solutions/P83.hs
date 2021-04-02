{- |
Description: 'spanningTrees'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P83" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P83 (spanningTrees, isTree, isConnected) where

import qualified Data.Map.Lazy   as Map
import           Data.Maybe      (fromJust)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs

-- | Construct all spanning trees
--
-- Mathematically, a tree is a graph with exactly one path between any two vertexes.
-- A spanning tree of a graph is a tree which includes all vertexes of the graph.
--
-- Write a function to construct all spanning trees of a given graph.
spanningTrees :: G -> [G]
spanningTrees g@(G m)
  | Map.null m = []
  | otherwise = expandFront g (Set.singleton v) (Set.singleton v, Set.empty)
  where (v, _) = Map.findMin m

-- | Write a function which returns whether a graph is a tree using 'spanningTrees'.
isTree :: G -> Bool
isTree g
  | Set.null (vertexes g) = True
  | otherwise             = length (spanningTrees g) == 1

-- | Write a function which returns whether a graph is connected using 'spanningTrees'.
isConnected :: G -> Bool
isConnected g
  | Set.null (vertexes g) = True
  | otherwise             = not $ null $ spanningTrees g

expandFront :: G -> Set Vertex -> (Set Vertex, Set Edge) -> [G]
expandFront g@(G m) front (vs, es)
  | null front = if Set.size vs == Map.size m then [fromJust $ toGraph (vs, es)] else []
  | otherwise = concat $ map expandTendrils $ filter disjoint $ Set.toList $ Set.powerSet tendrilsSet
  where tendrilsSet = Set.filter notPassed $ Set.filter (notVisited . snd) border
        notVisited v = not $ Set.member v vs
        notPassed e = not $ Set.member (Edge e) es
        border = Set.unions $ Set.map (\v -> Set.map (\v' -> (v,v')) $ neighbors v g) $ front
        expandTendrils tendrils = expandFront g (Set.map snd tendrils) (incorporateTendrils tendrils)
        incorporateTendrils tendrils = (Set.union vs $ Set.map snd tendrils, Set.union es $ Set.map Edge tendrils)

disjoint :: Set (Vertex, Vertex) -> Bool
disjoint s = fst $ Set.foldl accumulate (True, Set.empty) s
  where accumulate (r, vs) (_, v) = (r && not (Set.member v vs), Set.insert v vs)
