{-# OPTIONS_GHC -fno-warn-orphans #-}
module Problems.MultiwayTrees.Arbitrary (Arbitrary) where

import           Generic.Random
import           Problems.MultiwayTrees
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (MultiwayTree a) where
  arbitrary = genericArbitraryRec uniform
    `withBaseCase` (MultiwayTree <$> arbitrary <*> return [])
