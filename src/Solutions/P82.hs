{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

{- |
Description: Cycles with a given vertex
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P82" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P82 (cycles) where

import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs

-- | Finds all cycles in the graph which include the given vertex.
cycles :: Vertex -> G -> [[Vertex]]
cycles v g = map reverse $ start v g

-- | Start building up cycle from the first vertex.
start :: Vertex -> G -> [[Vertex]]
start v g = concat $ Set.map (\v' -> spread v g (v', [v], Set.singleton v, Set.empty)) $ neighbors v g

-- | Build up cycles one vertex at a time.
spread :: Vertex -> G -> (Vertex, [Vertex], Set Vertex, Set Edge) -> [[Vertex]]
spread target graph (v, path, visited, passed)
  | Set.member e passed = []
  | v == target = [path]
  | Set.member v visited = []
  | otherwise = concat $ Set.map continue $ neighbors v graph
  where e = Edge (v, head path)
        continue v' = spread target graph (v', path', visited', passed')
        path' = v : path
        visited' = Set.insert v visited
        passed' = Set.insert e passed
