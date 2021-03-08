{- |
Description: 'isomorphic'

Some solutions to "Problems.P85" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P85 (isomorphic) where

import           Data.List       (permutations)
import           Data.Map        (Map, (!))
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs

-- | Graph isomorphism.
--
-- Two graphs \(G_1 = (V_1,E_1)\) and \(G_2 = (V_2,E_2)\) are isomorphic if there is
-- a bijection \(f: V1 -> V_2\) such that for any vertexes \(x\), \(y\) in \(V_1\),
-- \(x\) and \(y\) are adjacent if and only if \(f(x)\) and \(f(y)\) are adjacent.
--
-- Write a function that determines whether two graphs are isomorphic.
isomorphic :: G -> G -> Bool
isomorphic g g'
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
