{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.BinaryTrees.QuickCheck ( treesOf, shrinkTree) where

import           Problems.BinaryTrees
import           Test.QuickCheck

-- | Generates a binary tree.
--
-- Uses the given function to generate node values.
treesOf :: Gen a -> Gen (Tree a)
treesOf g = sized gen
  where gen 0 = frequency [ (10, empty), (1, tree) ]
        gen _ = frequency [ (1, empty), (10, tree) ]

        empty = pure Empty

        tree = Branch <$> g <*> subtree <*> subtree
          where subtree = scale (`div` 2) $ treesOf g

-- | Shrink a binary tree.
--
-- Uses the given shrinking function to shrink node values.
shrinkTree :: (a -> [a]) -> Tree a -> [Tree a]
shrinkTree _ Empty = []
shrinkTree shrinkValue (Branch x l r) =
  [ Empty, l, r ] ++
  [ Branch y l r | y <- shrinkValue x ] ++
  [ Branch x l' r' | l' <- shrinkTree shrinkValue l, r' <- shrinkTree shrinkValue r ]
