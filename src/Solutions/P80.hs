{- |
Description: Converting between graph representations
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P80" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P80 (ConvertibleGraph, toLists, toAdjacency, toPaths, toG) where

import           Data.Maybe      (fromJust)
import           Problems.Graphs

-- | Write functions to convert between the different graph representations
-- 'Lists', 'Adjacency', 'Paths', and 'G'.
--
-- The types can already be easily converted between each other using
-- the 'sets' and 'toGraph' functions available to the 'Graph' type class.
-- Unlike the other graph problems, this problem should be solved without using
-- the functions available to the 'Graph' type class for it to not be trivial.
class Graph g => ConvertibleGraph g where
  -- | Convert graph to the 'Lists' representation.
  toLists :: g -> Lists
  toLists = fromJust . toGraph . sets

  -- | Convert graph to the 'Adjacency' representation.
  toAdjacency :: g -> Adjacency
  toAdjacency = fromJust . toGraph . sets

  -- | Convert graph to the 'Paths' representation.
  toPaths :: g -> Paths
  toPaths = fromJust . toGraph . sets

  -- | Convert graph to the 'G' representation.
  toG :: g -> G
  toG = fromJust . toGraph . sets

  -- All of the above do use 'toGraph' and 'sets', so the solutions are trivial here.
  -- In essence, it was still an interesting and non-trivial exercise for me,
  -- because I already did the real work as part of implementing 'toGraph' for
  -- each of the graph representations.

instance ConvertibleGraph Lists where
  toLists = id

instance ConvertibleGraph Adjacency where
  toAdjacency = id

instance ConvertibleGraph Paths where
  toPaths = id

instance ConvertibleGraph G where
  toG = id
