{- |
Description: Graph isomorphism
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P85".
-}
module Problems.P85 (isomorphic, graph85, graph85') where

import           Problems.Graphs
import           Problems.P80
import qualified Solutions.P85   as Solution

-- $setup
-- >>> import           Problems.Graphs
-- >>> import           Problems.P80

-- | Two graphs \(G_1 = (V_1,E_1)\) and \(G_2 = (V_2,E_2)\) are isomorphic if there is
-- a bijection \(f: V_1 -> V_2\) such that for any vertexes \(x\), \(y\) in \(V_1\),
-- \(x\) and \(y\) are adjacent if and only if \(f(x)\) and \(f(y)\) are adjacent.
--
-- Write a function that determines whether two graphs are isomorphic.
--
-- === Examples
--
-- >>> isomorphic (toG $ Paths [[1,2,3], [2,4]]) (toG $ Paths [[1,2,3],[1,4]])
-- False
--
-- >>> isomorphic graph85 graph85'
-- True
isomorphic :: G -> G -> Bool
isomorphic = Solution.isomorphic

-- | Example graph to try out 'isomorphic', along with 'graph85''.
graph85 :: G
graph85 = toG $ Paths [[1,5,3,7,1,6,2,5], [2,8,3], [6,4,8], [4,7]]

-- | Example graph to try out 'isomorphic', along with 'graph85'.
graph85' :: G
graph85' = toG $ Paths [[1,2,3,4,8,5,1], [5,6,7,8], [2,6], [3,7], [1,4]]
