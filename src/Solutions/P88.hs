{- |
Description: 'connectedComponents'
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P88" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P88 (connectedComponents) where

import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs
import           Problems.P87

-- | Connected components.
--
-- Write a function that splits a graph into its connected components.
connectedComponents :: G -> [[Vertex]]
connectedComponents g = getComponents g (vertexes g) []

getComponents :: G -> Set Vertex -> [[Vertex]] -> [[Vertex]]
getComponents g vs cs
  | Set.null vs = cs
  | otherwise   = getComponents g vs' (c:cs)
  where v = Set.findMin vs  -- chosen arbitrarily
        c = depthFirst g v
        vs' = Set.difference vs $ Set.fromList c
