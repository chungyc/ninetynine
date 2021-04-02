{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.Lists.Arbitrary (Arbitrary) where

import           Generic.Random
import           Problems.Lists
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (NestedList a) where
  arbitrary = genericArbitraryRec uniform `withBaseCase` (Elem <$> arbitrary)
