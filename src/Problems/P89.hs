{- |
Description: Bipartite graphs
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P88".
-}
module Problems.P89 (bipartite) where

import           Problems.Graphs
import qualified Solutions.P89   as Solution

-- $setup
-- >>> import Problems.Graphs
-- >>> import Problems.P80

-- | Write a function that finds out whether a given graph is bipartite.
--
-- === Examples
--
-- >>> bipartite $ toG $ Paths [[1,2,3,4],[1,4,5,2]]
-- True
--
-- >>> bipartite $ toG $ Paths [[1,2,3,4],[1,4,5,2],[1,3]]
-- False
bipartite :: G -> Bool
bipartite = Solution.bipartite
