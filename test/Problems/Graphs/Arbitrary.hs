{-# OPTIONS_GHC -fno-warn-orphans #-}
module Problems.Graphs.Arbitrary (Arbitrary) where

import           Data.Maybe      (fromJust)
import           Data.Set
import           Problems.Graphs
import           Test.QuickCheck

instance Arbitrary G where
  arbitrary = scale (ceiling . scaledSize) $ do
    vs <- arbitrary
    es <- sublistOf [Edge (u, v) | u <- vs, v <- vs, u /= v]
    return $ fromJust $ toGraph (fromList vs, fromList es)
      -- For k vertexes, there can be about k^2 / 2 edges.
      where scaledSize n = (*) 2 $ sqrt $ fromIntegral n :: Float
