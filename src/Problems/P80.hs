{- |
Description: Graph representation conversions

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P80".
-}
module Problems.P80 (
  ListsConvertible (toLists),
  AdjacencyConvertible (toAdjacency),
  PathsConvertible (toPaths),
  GConvertible (toG),
  ) where

import           Problems.Graphs
import qualified Solutions.P80   as Solution

{-
For each target graph representation type, there is a type class for which
the other graph representation types are instances of.  If one is able to
use a single function definition to convert the other graph representation types
to the target type, this allows it to be shared for all source types with
the default function definition in the type class.

For example, if all the other types can be change into the 'Lists' type
using the same function definition, then there need be only a single
default function definition for 'toLists' in the 'ListsConvertible' type class.
The other types are instances of this type class, so the single function definition
will be used for all of them.

If the conversion has to be done differently for all target types,
then the default function definition for the 'ListsConvertible' type class can be removed.
Instead, there will be specific function definitions of 'toLists' in each instance declaration.
For example, it can be much more efficient to use a custom algorithm between
certain graph representations instead of using a generic approach.
-}

-- | Graphs which can be converted into the 'Lists' representation.
class (Graph g, Solution.ListsConvertible g) => ListsConvertible g where
  -- | Convert graph to the 'Lists' representation.
  toLists :: g -> Lists
  toLists = Solution.toLists

instance ListsConvertible Adjacency where
--  toLists = undefined

instance ListsConvertible Paths where
--  toLists = undefined

instance ListsConvertible G where
--  toLists = undefined

-- | Graphs which can be converted into the 'Adjacency' representation.
class (Graph g, Solution.AdjacencyConvertible g) => AdjacencyConvertible g where
  -- | Convert graph to the 'Adjacency' representation.
  toAdjacency :: g -> Adjacency
  toAdjacency = Solution.toAdjacency

instance AdjacencyConvertible Lists where
--  toAdjacency = undefined

instance AdjacencyConvertible Paths where
--  toAdjacency = undefined

instance AdjacencyConvertible G where
--  toAdjacency = undefined

-- | Graphs which can be converted into the 'Paths' representation.
class (Graph g, Solution.PathsConvertible g) => PathsConvertible g where
  -- | Convert graph to the 'Paths' representation.
  toPaths :: g -> Paths
  toPaths = Solution.toPaths

instance PathsConvertible Lists where
--  toPaths = undefined

instance PathsConvertible Adjacency where
--  toPaths = undefined

instance PathsConvertible G where
--  toPaths = undefined

-- | Graphs which can be converted into the 'G' representation.
class (Graph g, Solution.GConvertible g) => GConvertible g where
  -- | Convert graph to the 'G' representation.
  toG :: g -> G
  toG = Solution.toG

instance GConvertible Lists where
--  toG = undefined

instance GConvertible Adjacency where
--  toG = undefined

instance GConvertible Paths where
--  toG = undefined
