{- |
Description: 'isomorphic'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P85".
-}
module Problems.P85 (isomorphic) where

import           Problems.Graphs
import qualified Solutions.P85   as Solution

-- $setup
-- >>> import Problems.P80

-- | Graph isomorphism.
--
-- Two graphs \(G_1 = (V_1,E_1)\) and \(G_2 = (V_2,E_2)\) are isomorphic if there is
-- a bijection \(f: V1 -> V_2\) such that for any vertexes \(x\), \(y\) in \(V_1\),
-- \(x\) and \(y\) are adjacent if and only if \(f(x)\) and \(f(y)\) are adjacent.
--
-- Write a function that determines whether two graphs are isomorphic.
--
-- === Examples
--
-- >>> isomorphic (toG $ Paths [[1,2,3], [2,4]]) (toG $ Paths [[1,2,3],[1,4]])
-- False
--
-- >>> :{
--   isomorphic
--     (toG $ Lists
--       ([1,2,3,4,5,6,7,8],
--        [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]))
--     (toG $ Lists
--       ([1,2,3,4,5,6,7,8],
--        [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]))
-- :}
-- True
isomorphic :: G -> G -> Bool
isomorphic = Solution.isomorphic
