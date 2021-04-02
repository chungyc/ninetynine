{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P82Spec (spec) where

import           Data.List                 (nub)
import           Data.Maybe                (fromJust)
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary
import           Problems.P80
import qualified Problems.P82              as Problem
import qualified Solutions.P82             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Vertex -> G -> [[Vertex]]) -> String -> Spec
properties cycles name = do
  -- Cap size to limit combinatorial explosion.
  describe name $ modifyMaxSize (const 40) $ do
    prop "includes cycle" $ do
      \(Sets (vs, es)) -> do
        vs' <- sublistOf $ Set.toList vs
        return $ forAll (shuffle vs') $ \trail ->
          length (nub trail) > 2 ==>
          head trail /= last trail ==>
          let path = nub trail
              v = head path
              cycleEdges = Set.insert (Edge (last path, v)) $ pathEdges path
              es' = Set.union es $ cycleEdges  -- ensure cycle exists
          in cycles v (fromJust $ toGraph (vs, es')) `shouldSatisfy` elem path

    prop "does not include non-existant path" $ do
      \(Sets (vs, es)) -> do
        vs' <- sublistOf $ Set.toList vs
        return $ forAll (shuffle vs') $ \trail ->
          length trail > 1 ==>
          head trail /= last trail ==>
          let path = nub trail
              v = head path
              cycleEdges = Set.insert (Edge (last path, v)) $ pathEdges path
              es' = Set.difference es $ cycleEdges  -- ensure cycle does not exist
          in cycles v (fromJust $ toGraph (vs, es')) `shouldNotSatisfy` elem path

  where pathEdges []             = Set.empty
        pathEdges [_]            = Set.empty
        pathEdges (u : vs@(v:_)) = Set.insert (Edge (u, v)) $ pathEdges vs

examples :: Spec
examples = do
  describe "examples" $ do
    it "cycles 1 $ toG $ Paths [[1,2,3], [1,3,4,2], [5,6]]" $ do
      cycles 1 (toG $ Paths [[1,2,3], [1,3,4,2], [5,6]])
        `shouldMatchList` [[1,2,3], [1,2,4,3], [1,3,2], [1,3,4,2]]

  where cycles = Problem.cycles

spec :: Spec
spec = parallel $ do
  properties Problem.cycles "cycles"
  examples
  describe "From solutions" $ do
    properties Solution.cycles "cycles"
