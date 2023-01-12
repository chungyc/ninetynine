{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.MultiwayTrees.Arbitrary (Arbitrary) where

import           Problems.MultiwayTrees
import           Problems.MultiwayTrees.QuickCheck
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (MultiwayTree a) where
  arbitrary = multiwayTreesOf arbitrary
  shrink = shrinkMultiwayTree shrink
