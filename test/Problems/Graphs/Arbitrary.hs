{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.Graphs.Arbitrary (Arbitrary) where

import           Data.Maybe                 (fromJust)
import           Data.Set                   (Set)
import           Problems.Graphs
import           Problems.Graphs.QuickCheck
import           Test.QuickCheck

instance Arbitrary Lists where
  arbitrary = fromJust . toGraph . fromSets <$> arbitrary
  shrink g = map (fromJust . toGraph . fromSets) $ shrink $ Sets $ sets g

instance Arbitrary Adjacency where
  arbitrary = fromJust . toGraph . fromSets <$> arbitrary
  shrink g = map (fromJust . toGraph . fromSets) $ shrink $ Sets $ sets g

instance Arbitrary Paths where
  arbitrary = fromJust . toGraph . fromSets <$> arbitrary
  shrink g = map (fromJust . toGraph . fromSets) $ shrink $ Sets $ sets g

instance Arbitrary G where
  arbitrary = fromJust . toGraph . fromSets <$> arbitrary
  shrink g = map (fromJust . toGraph . fromSets) $ shrink $ Sets $ sets g

fromSets :: Sets -> (Set Vertex, Set Edge)
fromSets (Sets s) = s
