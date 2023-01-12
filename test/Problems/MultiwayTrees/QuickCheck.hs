{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.MultiwayTrees.QuickCheck (multiwayTreesOf, shrinkMultiwayTree) where

import           Problems.MultiwayTrees
import           Test.QuickCheck

-- | Generates a multiway tree.
--
-- Uses the given function to generate node values.
multiwayTreesOf :: Gen a -> Gen (MultiwayTree a)
multiwayTreesOf g = sized gen
  where gen 0 = frequency
                [ (10, MultiwayTree <$> g <*> return [])
                , (1, MultiwayTree <$> g <*> subtreeList 0)
                ]
        gen n = frequency
                [ (1, MultiwayTree <$> g <*> return [])
                , (10, MultiwayTree <$> g <*> subtreeList n)
                ]
        subtreeList n = do
          k <- chooseInt (0, n)
          let s = case k of 0 -> 0; _ -> n `div` k
          vectorOf k $ gen s

-- | Shrink a multiway tree.
--
-- Uses the given shrinking function to shrink node values.
shrinkMultiwayTree :: (a -> [a]) -> MultiwayTree a -> [MultiwayTree a]
shrinkMultiwayTree _ (MultiwayTree _ []) = []
shrinkMultiwayTree shrinkValue (MultiwayTree x xs) =
  [ MultiwayTree x [] ] ++ xs ++
  [ MultiwayTree x' xs | x' <- shrinkValue x ] ++
  [ MultiwayTree x xs' | xs' <- shrinkList (shrinkMultiwayTree shrinkValue) xs ]
