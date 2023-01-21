{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.Lists.Arbitrary (Arbitrary) where

import           Problems.Lists
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (NestedList a) where
  arbitrary = gen
    where gen = sized $ \n -> case n of
            0 -> Elem <$> arbitrary
            _ -> oneof [ Elem <$> arbitrary
                       , List <$> scale (`div` 2) (listOf gen)
                       ]

  shrink = genericShrink
