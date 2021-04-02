{- |
Description: Graph representation conversions
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P80".
-}
module Problems.P80 (ConvertibleGraph, toLists, toAdjacency, toPaths, toG) where

import           Problems.Graphs
import qualified Solutions.P80   as Solution

-- | Write functions to convert between the different graph representations
-- 'Lists', 'Adjacency', 'Paths', and 'G'.
--
-- The types can already be easily converted between each other using
-- the 'sets' and 'toGraph' functions available to the 'Graph' type class.
-- Unlike the other graph problems, this problem should be solved without using
-- the functions available to the 'Graph' type class for it to not be trivial.
class (Graph g, Solution.ConvertibleGraph g) => ConvertibleGraph g where
  -- | Convert graph to the 'Lists' representation.
  toLists :: g -> Lists
  toLists = Solution.toLists

  -- | Convert graph to the 'Adjacency' representation.
  toAdjacency :: g -> Adjacency
  toAdjacency = Solution.toAdjacency

  -- | Convert graph to the 'Paths' representation.
  toPaths :: g -> Paths
  toPaths = Solution.toPaths

  -- | Convert graph to the 'G' representation.
  toG :: g -> G
  toG = Solution.toG

{-
One can either include a single function definition which can handle
conversions from all graph representations to a particular representation,
or include specific function definitions between particular pairs of graph representations.

I.e., if one can and wants to implement a single function definition of 'toLists',
its function definition can be defined once in the ConvertibleGraph type class above.
If one wishes to use a particular function definition for 'toLists' from a 'G' graph representation,
once can include it in the instance declaration for 'G' below.
-}

instance ConvertibleGraph Lists where
  toLists = id
  -- toAdjacency = undefined
  -- toPaths = undefined
  -- toG = undefined

instance ConvertibleGraph Adjacency where
  -- toLists = undefined
  toAdjacency = id
  -- toPaths = undefined
  -- toG = undefined

instance ConvertibleGraph Paths where
  -- toLists = undefined
  -- toAdjacency = undefined
  toPaths = id
  -- toG = undefined

instance ConvertibleGraph G where
  -- toLists = undefined
  -- toAdjacency = undefined
  -- toPaths = undefined
  toG = id
