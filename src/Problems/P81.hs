{- |
Description: 'paths'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P81".
-}
module Problems.P81 (paths) where

import           Problems.Graphs
import qualified Solutions.P81   as Solution

-- $setup
-- >>> import Data.List
-- >>> import Problems.P80

-- | Paths from one node to another one
--
-- Write a function that, given two vertexes @a@ and @b@ in a graph,
-- returns all the acyclic paths from @a@ to @b@.
--
-- === Examples
--
-- >>> sort $ paths 1 4 $ toG $ Paths [[1,2,3], [1,3,4,2], [5,6]]
-- [[1,2,3,4],[1,2,4],[1,3,2,4],[1,3,4]]
--
-- >>> paths 2 6 $ toG $ Paths [[1,2,3], [1,3,4,2], [5,6]]
-- []
paths :: Vertex -> Vertex -> G -> [[Vertex]]
paths = Solution.paths
