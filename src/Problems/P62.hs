{- |
Description: `atLevel`
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P62".
-}
module Problems.P62 (atLevel) where

import           Problems.BinaryTrees
import qualified Solutions.P62        as Solution

-- $setup
-- >>> import Problems.P54

-- | Collect the nodes at a given level in a list
--
-- A node of a binary tree is at level \(n\) if the path from the root to the node has length \(n-1\).
-- The root node is at level 1.
--
-- === Examples
--
-- >>> atLevel tree4 2
-- [2,2]
--
-- === __Notes__
--
-- In the original list, this was problem 62B, and 'Problems.P61.internals' was problem 62,
-- but the latter was grouped with problem 61 because the grouping was more natural.
atLevel :: Tree a -> Int -> [a]
atLevel = Solution.atLevel
