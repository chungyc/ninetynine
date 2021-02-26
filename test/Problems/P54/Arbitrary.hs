{-# OPTIONS_GHC -fno-warn-orphans #-}
module Problems.P54.Arbitrary (Arbitrary) where

import           Generic.Random
import           Problems.P54.Definitions
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genericArbitraryRec uniform `withBaseCase` return Empty
