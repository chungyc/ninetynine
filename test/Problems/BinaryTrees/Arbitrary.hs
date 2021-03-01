{-# OPTIONS_GHC -fno-warn-orphans #-}
module Problems.BinaryTrees.Arbitrary (Arbitrary) where

import           Generic.Random
import           Problems.BinaryTrees
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genericArbitraryRec uniform `withBaseCase` return Empty
