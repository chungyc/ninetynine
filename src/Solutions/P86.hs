{- |
Description: Graph coloring
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P86" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P86 (colorGraph) where

import           Data.List       (sortOn)
import           Data.Map.Lazy   (Map)
import qualified Data.Map.Lazy   as Map
import qualified Data.Set        as Set
import           Problems.Graphs

-- | Color a graph
-- using [Welch-Powell's algorithm](https://graphstream-project.org/doc/Algorithms/Welsh-Powell/).
-- Uses distinct integers to represent distinct colors,
-- and returns the association list between vertexes and their colors.
colorGraph :: G -> [(Vertex, Int)]
colorGraph g = Map.toList $ colorVertexes g (sortByDegree g) 1 Map.empty

colorVertexes :: G -> [Vertex] -> Int -> Map Vertex Int -> Map Vertex Int
colorVertexes _ [] _ coloring = coloring
colorVertexes g (v:vs) color coloring = colorVertexes g remaining (color+1) coloring'
  where (remaining, coloring') = colorOthers g v vs color ([], Map.insert v color coloring)

colorOthers :: G -> Vertex -> [Vertex] -> Int -> ([Vertex], Map Vertex Int) -> ([Vertex], Map Vertex Int)
colorOthers _ _ [] _ (uncolored, coloring) = (reverse uncolored, coloring)
colorOthers g v (v':vs') color (uncolored,coloring)
  | neighborsHaveColor g color coloring v' = colorOthers g v vs' color (v' : uncolored, coloring)
  | otherwise                              = colorOthers g v vs' color (uncolored, Map.insert v' color coloring)

neighborsHaveColor :: G -> Int -> Map Vertex Int -> Vertex -> Bool
neighborsHaveColor g color coloring v = any (hasColor color coloring) $ neighbors v g

hasColor :: Int -> Map Vertex Int -> Vertex -> Bool
hasColor color coloring v = isColor $ Map.lookup v coloring
  where isColor Nothing  = False
        isColor (Just c) = color == c

-- In descending order.
sortByDegree :: G -> [Vertex]
sortByDegree g = sortOn ((-) 0 . Set.size . flip neighbors g) vs
  where vs = Set.toList $ vertexes g
