{- |
Description: `leaves`

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P60".
-}
module Problems.P61 (leaves) where

import           Problems.BinaryTrees
import qualified Solutions.P61        as Solution

-- $setup
-- >>> import Problems.P54

-- | Collect the leaves of a binary tree in a list.  A leaf is a node with no successors.
--
-- === Examples
--
-- >>> leaves tree4
-- [4,2]
--
-- === __Notes__
--
-- The original problem also included implementing a function which counts leaves.
leaves :: Tree a -> [a]
leaves = Solution.leaves
