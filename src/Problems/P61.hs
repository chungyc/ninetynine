{- |
Description: Collect nodes of a binary tree
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P61".
-}
module Problems.P61 (leaves,internals) where

import           Problems.BinaryTrees
import qualified Solutions.P61        as Solution

-- $setup
-- >>> import Data.List (sort)
-- >>> import Problems.P54

-- | Collect the leaves of a binary tree in a list.  A leaf is a node with no successors.
--
-- === Examples
--
-- >>> sort $ leaves tree4
-- [2,4]
--
-- === __Notes__
--
-- The original problem also included implementing a function which counts leaves.
-- Instead, the 'internals' function was moved from problem 62 to this one
-- because it seemed a more natural grouping.
--
-- The examples sort the results due avoid order sensitivity.
-- It is less of an issue for 'leaves', which has a sort of obvious natural order,
-- but there is no single natural order for 'internals'.
leaves :: Tree a -> [a]
leaves = Solution.leaves

-- | Collect the internal nodes of a binary tree in a list.
--
-- An internal node of a binary tree has either one or two non-empty successors.
--
-- === Examples
--
-- >>> sort $ internals tree4
-- [1,2]
internals :: Tree a -> [a]
internals = Solution.internals
