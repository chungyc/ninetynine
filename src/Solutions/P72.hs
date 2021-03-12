{- |
Description: `postOrderSequence`

Some solutions to "Problems.P72" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P72 (postOrderSequence) where

import           Problems.MultiwayTrees

-- | Construct the post-order sequence of the tree nodes.
postOrderSequence :: MultiwayTree a -> [a]
postOrderSequence t = reverse $ addToList t []

-- | Add each node once to a list in post-order sequence.
-- List will be reverse post-order.
addToList :: MultiwayTree a -> [a] -> [a]
addToList (MultiwayTree x ts) xs = x:xs'
  where xs' = addToList' ts xs

addToList' :: [MultiwayTree a] -> [a] -> [a]
addToList' [] xs = xs
addToList' (t:ts) xs = addToList' ts xs'
  where xs' = addToList t xs
