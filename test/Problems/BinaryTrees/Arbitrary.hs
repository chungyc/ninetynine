{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.BinaryTrees.Arbitrary (Arbitrary) where

import           Generic.Random
import           Problems.BinaryTrees
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genericArbitraryRec uniform `withBaseCase` return Empty
  shrink = genericShrink
