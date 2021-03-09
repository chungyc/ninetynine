{- |
Description: `heightBalancedTreesWithNodes`

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P60".
-}
module Problems.P60 (heightBalancedTreesWithNodes) where

import           Problems.BinaryTrees
import qualified Solutions.P60        as Solution

-- | Construct all the height-balanced binary trees with a given number of nodes.
--
-- === Examples
--
-- >>> length $ heightBalancedTreesWithNodes 15
-- 1553
--
-- >>> mapM_ printTreeList $ map heightBalancedTreesWithNodes [0..3]
-- [ Empty]
-- [ Branch () Empty Empty]
-- [ Branch () (Branch () Empty Empty) Empty
-- , Branch () Empty (Branch () Empty Empty)]
-- [ Branch () (Branch () Empty Empty) (Branch () Empty Empty)]
--
-- === __Hint__
--
-- Consider a height-balanced binary tree of height \(h\).
-- What is the maximum number of nodes it can contain? Clearly, it is \(2^h-1\).
--
-- However, what is the minimum number of nodes it can contain?  This question is more difficult.
-- Try to find a recursive statement and turn it into a function
-- that returns the minimum number of nodes in a height-balanced binary tree of height \(h\).
--
-- On the other hand, what is the maximum height \(h\) a height-balanced binary tree
-- with \(n\) nodes can have?  What about the minimum height?  Try writing functions to compute these.
--
-- === __Notes__
--
-- The original problem asked to implement functions computing the minimum number of nodes for a given height
-- and the maximum height for a given number of nodes.  These are more of a hint as to how to implement
-- the main function in question, so they were made into hints instead of being part of the problem itself.
heightBalancedTreesWithNodes :: Int -> [Tree ()]
heightBalancedTreesWithNodes = Solution.heightBalancedTreesWithNodes
