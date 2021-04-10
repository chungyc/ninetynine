{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.MultiwayTrees.Arbitrary (Arbitrary) where

import           Generic.Random
import           Problems.MultiwayTrees
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (MultiwayTree a) where
  arbitrary = genericArbitraryRec uniform
    `withBaseCase` (MultiwayTree <$> arbitrary <*> return [])

  shrink = genericShrink
