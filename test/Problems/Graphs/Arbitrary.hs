{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.Graphs.Arbitrary (Sets (Sets), Arbitrary) where

import           Data.Maybe      (fromJust)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs
import           Test.QuickCheck

instance Arbitrary Lists where
  arbitrary = (fromJust . toGraph . fromSets) <$> arbitrary
  shrink g = map (fromJust . toGraph . fromSets) $ shrink $ Sets $ sets g

instance Arbitrary Adjacency where
  arbitrary = (fromJust . toGraph . fromSets) <$> arbitrary
  shrink g = map (fromJust . toGraph . fromSets) $ shrink $ Sets $ sets g

instance Arbitrary Paths where
  arbitrary = (fromJust . toGraph . fromSets) <$> arbitrary
  shrink g = map (fromJust . toGraph . fromSets) $ shrink $ Sets $ sets g

instance Arbitrary G where
  arbitrary = (fromJust . toGraph . fromSets) <$> arbitrary
  shrink g = map (fromJust . toGraph . fromSets) $ shrink $ Sets $ sets g

-- | Helper type for generating sets of vertexes and edges for a valid graph.
--
-- Example:
--
-- > spec :: Spec
-- > spec = do
-- >   prop "sets composed with toGraph is identity" $
-- >     \(Sets (vs, es)) -> sets (fromJust $ toGraph' (vs, es)) `shouldBe` (vs, es)
newtype Sets = Sets (Set Vertex, Set Edge)
  deriving Show

fromSets :: Sets -> (Set Vertex, Set Edge)
fromSets (Sets s) = s

instance Arbitrary Sets where
  arbitrary = scale (ceiling . scaledSize) $ do
    vs <- (\n -> [1..n]) <$> getSize
    es <- sublistOf [Edge (u, v) | u <- vs, v <- vs, u < v]
    return $ Sets (Set.fromList vs, Set.fromList es)
      -- For k vertexes, there can be about k^2 / 2 edges.
      where scaledSize n = (*) 2 $ sqrt $ fromIntegral n :: Float

  shrink (Sets (vs, es)) = map (\v -> Sets (Set.delete v vs, remove v)) $ Set.toList vs
    where remove v = Set.filter (\(Edge (u', v')) -> u' == v || v' == v) es
