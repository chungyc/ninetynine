{- |
Description: Post-order sequence of a tree
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P72".
-}
module Problems.P72 (postOrderSequence) where

import           Problems.MultiwayTrees
import qualified Solutions.P72          as Solution

-- | Construct the post-order sequence of the tree nodes.
--
-- === Examples
--
-- >>> postOrderSequence multitree5
-- "gfcdeba"
--
-- === __Notes__
--
-- The problem in the original list specifies a "bottom-up" order.
-- There is no widespread common understanding of what this is
-- (i.e., does this mean post-order, or all nodes at a level
-- being sequenced before nodes at a higher level?),
-- so post-order is specified instead.
postOrderSequence :: MultiwayTree a -> [a]
postOrderSequence = Solution.postOrderSequence
