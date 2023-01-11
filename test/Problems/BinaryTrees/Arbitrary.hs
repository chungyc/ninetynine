{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.BinaryTrees.Arbitrary (Arbitrary) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.QuickCheck
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = treesOf arbitrary
  shrink = shrinkTree shrink
