{- |
Description: `construct`

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P57".
-}
module Problems.P57 (construct, addedTo) where

import           Problems.P54.Definitions
import qualified Solutions.P57            as Solution

-- $setup
-- >>> import Problems.P56

-- | Binary search trees.
--
-- Write an 'addedTo' function which adds an element to a binary search tree to obtain another binary search tree.
-- Use this function to construct a binary search tree from a list of ordered elements.
-- Each element should be added from the front to the back of the list.
--
-- Examples:
--
-- >>> construct [3, 2, 5, 7, 1]
-- Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
--
-- >>> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
-- True
--
-- >>> symmetric . construct $ [3, 2, 5, 7, 1]
-- True
--
-- &#129335; &#129335;
--
-- === __Notes__
--
-- The original problem refers to a predicate which had not appeared in previous
-- problems: /Use the predicate add\/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers./
-- This is presumably a reference to course material used at the time the problems were written.
-- Given the different context, the problem was revised to also implement the function that would
-- have take the role that the @add\/3@ predicate would have had in the original context.
--
-- For consistent examples, the condition to also use naive binary search tree insertion was imposed.
-- For the same reason, the order for which elements in a list are added to the tree is also specified.
-- Of course, anyone is free to implement whatever they want, although that could mean they would not pass the available tests.
construct :: Ord a => [a] -> Tree a
construct = Solution.construct

-- | Return a binary search tree with an element added to another binary search tree.
--
-- Use a simple insertion algorithm which leaves the tree mostly the same except
-- for the new element in a leaf node.  In particular, no balancing should be done.
--
-- Example:
--
-- >>> 8 `addedTo` Branch 5 Empty (Branch 7 Empty (Branch 9 Empty Empty))
-- Branch 5 Empty (Branch 7 Empty (Branch 9 (Branch 8 Empty Empty) Empty))
addedTo :: Ord a => a -> Tree a -> Tree a
addedTo = Solution.addedTo
