{- |
Description: Symmetric and completely balanced binary trees
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P58".
-}
module Problems.P58 (symmetricBalancedTrees) where

import           Problems.BinaryTrees
import qualified Solutions.P58        as Solution

-- $setup
-- >>> import Problems.BinaryTrees

-- | Construct all symmetric, completely balanced binary trees with a given number of nodes.
--
-- === Examples
--
-- >>> printTreeList $ symmetricBalancedTrees 5
-- [ Branch () (Branch () (Branch () Empty Empty) Empty) (Branch () Empty (Branch () Empty Empty))
-- , Branch () (Branch () Empty (Branch () Empty Empty)) (Branch () (Branch () Empty Empty) Empty) ]
symmetricBalancedTrees :: Int -> [Tree ()]
symmetricBalancedTrees = Solution.symmetricBalancedTrees
