{- |
Description: Graph representation conversions

Some solutions to "Problems.P80" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P80 (
  ListsConvertible (toLists),
  AdjacencyConvertible (toAdjacency),
  PathsConvertible (toPaths),
  GConvertible (toG),
  ) where

import           Data.Maybe      (fromJust)
import           Problems.Graphs

-- | Graphs which can be converted into the 'Lists' representation.
class Graph g => ListsConvertible g where
  -- | Convert graph to the 'Lists' representation.
  toLists :: g -> Lists
  toLists = fromJust . toGraph . sets

instance ListsConvertible Adjacency where

instance ListsConvertible Paths where

instance ListsConvertible G where

-- | Graphs which can be converted into the 'Adjacency' representation.
class Graph g => AdjacencyConvertible g where
  -- | Convert graph to the 'Adjacency' representation.
  toAdjacency :: g -> Adjacency
  toAdjacency = fromJust . toGraph . sets

instance AdjacencyConvertible Lists where

instance AdjacencyConvertible Paths where

instance AdjacencyConvertible G where

-- | Graphs which can be converted into the 'Paths' representation.
class Graph g => PathsConvertible g where
  -- | Convert graph to the 'Paths' representation.
  toPaths :: g -> Paths
  toPaths = fromJust . toGraph . sets

instance PathsConvertible Lists where

instance PathsConvertible Adjacency where

instance PathsConvertible G where

-- | Graphs which can be converted into the 'G' representation.
class Graph g => GConvertible g where
  -- | Convert graph to the 'G' representation.
  toG :: g -> G
  toG = fromJust . toGraph . sets

instance GConvertible Lists where

instance GConvertible Adjacency where

instance GConvertible Paths where
