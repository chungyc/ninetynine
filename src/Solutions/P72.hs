{- |
Description: Post-order sequence of a tree
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P72" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P72 (postOrderSequence) where

import           Problems.MultiwayTrees

-- | Construct the post-order sequence of the tree nodes.
postOrderSequence :: MultiwayTree a -> [a]
postOrderSequence t = reverse $ addToList t []

-- | Add each node once to a list in post-order sequence.
-- List will end up as reverse post-order sequence.
addToList :: MultiwayTree a -> [a] -> [a]
addToList (MultiwayTree x ts) xs = x:xs'
  where xs' = addToList' ts xs

addToList' :: [MultiwayTree a] -> [a] -> [a]
addToList' [] xs = xs
addToList' (t:ts) xs = addToList' ts xs'
  where xs' = addToList t xs
