{-# OPTIONS_GHC -fno-warn-orphans #-}
module Problems.Lists.Arbitrary (Arbitrary) where

import           Generic.Random
import           Problems.Lists
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (NestedList a) where
  arbitrary = genericArbitraryRec uniform `withBaseCase` (Elem <$> arbitrary)
