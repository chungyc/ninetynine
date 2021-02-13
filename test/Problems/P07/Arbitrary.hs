{-# OPTIONS_GHC -fno-warn-orphans #-}
module Problems.P07.Arbitrary (Arbitrary) where

import           Generic.Random
import           Problems.P07.Definitions
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (NestedList a) where
  arbitrary = genericArbitraryRec uniform `withBaseCase` (Elem <$> arbitrary)
