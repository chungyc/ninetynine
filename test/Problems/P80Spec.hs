{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P80Spec (spec) where

import           Problems.Graphs
import           Problems.Graphs.Arbitrary
import           Problems.P80
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "toLists" $ do
    prop "converts to same valid graph from Lists" $
      withGraph $ \g -> toLists (g :: Lists) `convertsFrom` g

    prop "converts to same valid graph from Adjacency" $
      withGraph $ \g -> toLists (g :: Adjacency) `convertsFrom` g

    prop "converts to same valid graph from Paths" $
      withGraph $ \g -> toLists (g :: Paths) `convertsFrom` g

    prop "converts to same valid graph from G" $
      withGraph $ \g -> toLists (g :: G) `convertsFrom` g

  describe "toAdjacency" $ do
    prop "converts to same valid graph from Lists" $
      withGraph $ \g -> toAdjacency (g :: Lists) `convertsFrom` g

    prop "converts to same valid graph from Adjacency" $
      withGraph $ \g -> toAdjacency (g :: Adjacency) `convertsFrom` g

    prop "converts to same valid graph from Paths" $
      withGraph $ \g -> toAdjacency (g :: Paths) `convertsFrom` g

    prop "converts to same valid graph from G" $
      withGraph $ \g -> toAdjacency (g :: G) `convertsFrom` g

  describe "toPaths" $ do
    prop "converts to same graph from Lists" $
      withGraph $ \g -> toPaths (g :: Lists) `convertsFrom` g

    prop "converts to same graph from Adjacency" $
      withGraph $ \g -> toPaths (g :: Adjacency) `convertsFrom` g

    prop "converts to same graph from Paths" $
      withGraph $ \g -> toPaths (g :: Paths) `convertsFrom` g

    prop "converts to same graph from G" $
      withGraph $ \g -> toPaths (g :: G) `convertsFrom` g

  describe "toG" $ do
    prop "converts to same graph from Lists" $
      withGraph $ \g -> toG (g :: Lists) `convertsFrom` g

    prop "converts to same graph from Adjacency" $
      withGraph $ \g -> toG (g :: Adjacency) `convertsFrom` g

    prop "converts to same graph from Paths" $
      withGraph $ \g -> toG (g :: Paths) `convertsFrom` g

    prop "converts to same graph from G" $
      withGraph $ \g -> toG (g :: G) `convertsFrom` g

  -- There are no separate tests here for "Solutions.P80".
  -- Their important bits are already tested by "GraphsSpec".

  where convertsFrom g' g = conjoin [ sets g' `shouldBe` sets g, g' `shouldSatisfy` isValidGraph ]
