{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P82Spec (spec) where

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
properties cycles name = describe name $ modifyMaxSize (const 25) $ do
  prop "includes cycle" $ \(Sets (vs, es)) ->
    forAll (sublistOf $ Set.toList vs) $ \vs' ->
    forAll (shuffle vs') $ \path ->
    length path > 2 ==>
    let v = head path
        cycleEdges = Set.insert (Edge (last path, v)) $ pathEdges path
        es' = Set.union es cycleEdges  -- ensure cycle exists
        g = fromJust $ toGraph (vs, es')
    in counterexample ("from " ++ show v) $
       counterexample (show $ toPaths g) $
       cycles v g `shouldSatisfy` elem path

  prop "does not include non-existant path" $ \(Sets (vs, es)) ->
    forAll (sublistOf $ Set.toList vs) $ \vs' ->
    forAll (shuffle vs') $ \path ->
    length path > 1 ==>
    let v = head path
        cycleEdges = Set.insert (Edge (last path, v)) $ pathEdges path
        es' = Set.difference es $ cycleEdges  -- ensure cycle does not exist
        g = fromJust $ toGraph (vs, es')
    in counterexample ("from " ++ show v) $
       counterexample (show $ toPaths g) $
       cycles v g `shouldNotSatisfy` elem path

  where
    -- The set of edges forming the given path.
    pathEdges path = Set.fromList $ map Edge $ zip path (tail path)

examples :: Spec
examples = describe "examples" $ do
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
