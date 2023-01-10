{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.Logic.Arbitrary (Arbitrary) where

import           Problems.Logic
import           Problems.Logic.QuickCheck
import           Test.QuickCheck

instance Arbitrary Formula where
  arbitrary = formulas
  shrink = shrinkFormula
