{- |
Description: `construct`

Some solutions to "Problems.P57" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P57 (construct, addedTo) where

import           Problems.BinaryTrees

-- | Binary search trees.
--
-- Write an 'addedTo' function which adds an element to a binary search tree to obtain another binary search tree.
-- Use this function to construct a binary search tree from a list of ordered elements.
-- Each element should be added from the front to the back of the list.
construct :: Ord a => [a] -> Tree a
construct l = build l Empty
  where build [] tree     = tree
        build (x:xs) tree = build xs (x `addedTo` tree)

-- | Return a binary search tree with an element added to another binary search tree.
--
-- Use a simple insertion algorithm which leaves the tree mostly the same except
-- for the new element in a leaf node.  In particular, no balancing should be done.
addedTo :: Ord a => a -> Tree a -> Tree a
addedTo x Empty                 = Branch x Empty Empty
addedTo x (Branch y left right)
  | x < y = Branch y (x `addedTo` left) right
  | otherwise = Branch y left (x `addedTo` right)
