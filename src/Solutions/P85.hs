{- |
Description: Graph isomorphism
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P85" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P85 (isomorphic, isomorphic', isomorphic'') where

import           Data.List       (permutations, sortOn)
import           Data.Map        (Map, (!))
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs


-- | Two graphs \(G_1 = (V_1,E_1)\) and \(G_2 = (V_2,E_2)\) are isomorphic if there is
-- a bijection \(f: V1 -> V_2\) such that for any vertexes \(x\), \(y\) in \(V_1\),
-- \(x\) and \(y\) are adjacent if and only if \(f(x)\) and \(f(y)\) are adjacent.
--
-- Write a function that determines whether two graphs are isomorphic.
--
-- Builds up a bijection from a starting vertex to its neighbors,
-- expanding until it encounters an inconsistency or until there are no more vertexes to expand to.
isomorphic :: G -> G -> Bool
isomorphic g g'
  | not sameSize          = False
  | not sameDegrees       = False
  | Set.null $ vertexes g = Set.null $ vertexes g'
  | otherwise             = expand Map.empty (Set.singleton v) (g, vertexes g) (g', vertexes g')
  where v = maxDegreeVertex g $ vertexes g
        sameSize = Set.size (vertexes g) == Set.size (vertexes g')
        sameDegrees = isSameDegrees (classifyVertexDegrees g) (classifyVertexDegrees g')

-- | From the given non-empty set of vertexes, returns the vertex with the largest degree.
maxDegreeVertex :: G -> Set Vertex -> Vertex
maxDegreeVertex g vs = head $ reverse $ sortOn (Set.size . flip neighbors g) $ Set.toList $ vs

expand :: Map Vertex Vertex -> Set Vertex -> (G, Set Vertex) -> (G, Set Vertex) -> Bool
expand bijection frontier (g, vs) (g', vs')
  | Set.null vs = True
  | Set.null frontier = expand bijection (Set.singleton $ Set.findMin vs) (g, vs) (g', vs')
  | otherwise = any (expand' bijection frontier (g, vs) (g', vs') v) us
  where v = maxDegreeVertex g frontier
        us = Set.filter (isMatch g g' bijection v) vs'

expand' :: Map Vertex Vertex -> Set Vertex -> (G, Set Vertex) -> (G, Set Vertex) -> Vertex -> Vertex -> Bool
expand' bijection frontier (g, vs) (h, us) v u = expand bijection' frontier' (g, vs') (h, us')
  where bijection' = Map.insert v u bijection
        frontier' = Set.union (Set.delete v frontier) $ Set.intersection vs' $ neighbors v g
        vs' = Set.delete v vs
        us' = Set.delete u us

isMatch :: G -> G -> Map Vertex Vertex -> Vertex -> Vertex -> Bool
isMatch g g' bijection v v' = equalDegrees && consistentNeighbors
  where equalDegrees = Set.size (neighbors v g) == Set.size (neighbors v' g')
        consistentNeighbors = all (isNeighbor . flip Map.lookup bijection) $ neighbors v g
        isNeighbor Nothing  = True
        isNeighbor (Just u) = Set.member u $ neighbors v' g'

classifyVertexDegrees :: G -> Map Int (Set Vertex)
classifyVertexDegrees (G m) = Map.fromListWith Set.union l
  where l = map (\v -> (Set.size $ m ! v, Set.singleton v)) $ Map.keys m

isSameDegrees :: Map Int (Set Vertex) -> Map Int (Set Vertex) -> Bool
isSameDegrees m m' =
  Map.keys m == Map.keys m' &&
  all (\d -> Set.size (m ! d) == Set.size (m' ! d)) (Map.keys m)

-- | Two graphs \(G_1 = (V_1,E_1)\) and \(G_2 = (V_2,E_2)\) are isomorphic if there is
-- a bijection \(f: V1 -> V_2\) such that for any vertexes \(x\), \(y\) in \(V_1\),
-- \(x\) and \(y\) are adjacent if and only if \(f(x)\) and \(f(y)\) are adjacent.
--
-- Write a function that determines whether two graphs are isomorphic.
--
-- This tests all bijections of vertexes from one graph to another to see if any are identical.
isomorphic' :: G -> G -> Bool
isomorphic' g g'
  | Set.size vs /= Set.size vs' = False
  | otherwise = any (\p -> (vs, es) == permute (vs', es') vl p) $ permutations vl'
  where (vs, es) = sets g
        (vs', es') = sets g'
        vl = Set.toList vs
        vl' = Set.toList vs'

permute :: (Set Vertex, Set Edge) -> [Vertex] -> [Vertex] -> (Set Vertex, Set Edge)
permute (vs, es) vl vl' = (vs', es')
  where translate = Map.fromList $ zip vl vl'
        vs' = mapVertexes translate vs
        es' = mapEdges translate es

mapVertexes :: Map Vertex Vertex -> Set Vertex -> Set Vertex
mapVertexes translate vs = Set.map (translate !) vs

mapEdges :: Map Vertex Vertex -> Set Edge -> Set Edge
mapEdges translate es = Set.map (\(Edge (u, v)) -> Edge (f u, f v)) es
  where f = (!) translate

-- | Two graphs \(G_1 = (V_1,E_1)\) and \(G_2 = (V_2,E_2)\) are isomorphic if there is
-- a bijection \(f: V1 -> V_2\) such that for any vertexes \(x\), \(y\) in \(V_1\),
-- \(x\) and \(y\) are adjacent if and only if \(f(x)\) and \(f(y)\) are adjacent.
--
-- Write a function that determines whether two graphs are isomorphic.
--
-- Tests bijections which are limited to matching vertexes with the same degree,
-- and looks for any which results in one graph becoming identical to the other.
isomorphic'' :: G -> G -> Bool
isomorphic'' g g' =
  isSameDegrees degrees degrees' &&
  any (\p -> permute' (vs, es) p == (vs', es')) bijections
  where degrees = classifyVertexDegrees g
        degrees' = classifyVertexDegrees g'
        (vs, es) = sets g
        (vs', es') = sets g'
        groupByDegree m = map Set.toAscList $ Map.elems m
        bijections = combineLists (groupByDegree degrees) (groupByDegree degrees')

permute' :: (Set Vertex, Set Edge) -> [(Vertex, Vertex)] -> (Set Vertex, Set Edge)
permute' (vs, es) p = (vs', es')
  where translate = Map.fromList p
        vs' = mapVertexes translate vs
        es' = mapEdges translate es

combineLists :: [[Vertex]] -> [[Vertex]] -> [[(Vertex,Vertex)]]
combineLists [] [] = [[]]
combineLists (l:ls) (l':ls') = concat $ map (\p -> map (\b -> b ++ p) bijections) $ combineLists ls ls'
  where bijections = map (zip l) $ permutations l'
combineLists _ _ = undefined
