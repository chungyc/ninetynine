{- |
Description: `regularGraphs`

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P94".
-}
module Problems.P94 (regularGraphs) where

import           Problems.Graphs
import qualified Solutions.P94   as Solution

{- |
Generate \(k\)-regular graphs with \(n\) vertexes.

In a \(k\)-regular graph, all vertexes have a degree of \(k\).
I.e., the number of edges incident in each vertex is \(k\).
How many non-isomorphic 3-regular graphs with 6 vertexes are there?

=== Examples

>>> length $ regularGraphs 6 3
2
-}
regularGraphs :: Int  -- ^ \(n\)
              -> Int  -- ^ \(k\)
              -> [G]  -- ^ non-isomorphic \(k\)-regular graphs with \(n\) vertexes
regularGraphs = Solution.regularGraphs
