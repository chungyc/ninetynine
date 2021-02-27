{-# OPTIONS_GHC -fno-warn-orphans #-}
module Problems.Graphs.Arbitrary (Arbitrary) where

import           Data.Maybe      (fromJust)
import           Data.Set
import           Problems.Graphs
import           Test.QuickCheck

instance Arbitrary G where
  arbitrary = scale ((*) 2 . ceiling . sqrt . fromIntegral) $ do
    vs <- arbitrary
    es <- sublistOf [Edge (u, v) | u <- vs, v <- vs, u /= v]
    return $ fromJust $ toGraph (fromList vs, fromList es)
