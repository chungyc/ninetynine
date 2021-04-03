{- |
Description: Paths between vertexes
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P81" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P81 (paths) where

import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs

-- | Write a function that, given two vertexes @a@ and @b@ in a graph,
-- returns all the acyclic paths from @a@ to @b@.
paths :: Vertex -> Vertex -> G -> [[Vertex]]
paths a b g = map reverse $ spread a b g (Set.empty, [])

spread :: Vertex -> Vertex -> G -> (Set Vertex, [Vertex]) -> [[Vertex]]
spread a b g (visited, path)
  | a == b               = [b : path]
  | Set.member a visited = []
  | otherwise            = concat $ Set.map (\v -> spread v b g (visited', path')) $ neighbors a g
  where visited' = Set.insert a visited
        path' = a : path
