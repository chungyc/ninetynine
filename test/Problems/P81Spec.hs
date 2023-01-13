{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P81Spec (spec) where

import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as Set
import           Problems.Graphs
import           Problems.Graphs.QuickCheck
import           Problems.P80
import qualified Problems.P81               as Problem
import qualified Solutions.P81              as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Vertex -> Vertex -> G -> [[Vertex]]) -> String -> Spec
properties paths name = describe name $ modifyMaxSize (const 25) $ do
  prop "includes path" $ \(Sets (vs, es)) ->
    forAll (sublistOf $ Set.toList vs) $ \vs' ->
    forAll (shuffle vs') $ \path ->
    not (null path) ==>
    classify (length path <= 2) "trivial" $
    let src = head path
        dst = last path
        es' = Set.union es $ pathEdges path  -- ensure path exists
        g = fromJust $ toGraph (vs, es')
    in counterexample ("from " ++ show src ++ " to " ++ show dst) $
       counterexample (show $ toPaths g) $
       paths src dst g `shouldSatisfy` elem path

  prop "does not include non-existant path" $ \(Sets (vs, es)) ->
    forAll (sublistOf $ Set.toList vs) $ \vs' ->
    forAll (shuffle vs') $ \path ->
    length path > 1 ==>
    let src = head path
        dst = last path
        es' = Set.difference es $ pathEdges path  -- ensure path does not exist
        g = fromJust $ toGraph (vs, es')
    in counterexample ("from " ++ show src ++ " to " ++ show dst) $
       counterexample (show $ toPaths g) $
       paths src dst g `shouldNotSatisfy` elem path

  where
    -- The set of edges forming the given path.
    pathEdges path = Set.fromList $ map Edge $ zip path (tail path)

examples :: Spec
examples = describe "Examples" $ do
  it "paths 1 4 $ toG $ Paths [[1,2,3], [1,3,4,2], [5,6]]" $ do
    paths 1 4 (toG $ Paths [[1,2,3], [1,3,4,2], [5,6]])
      `shouldMatchList` [[1,2,4], [1,2,3,4], [1,3,2,4], [1,3,4]]

  it "paths 2 6 $ toG $ Paths [[1,2,3], [1,3,4,2], [5,6]]" $ do
    paths 2 6 (toG $ Paths [[1,2,3], [1,3,4,2], [5,6]]) `shouldBe` []

  where paths = Problem.paths

spec :: Spec
spec = parallel $ do
  properties Problem.paths "paths"
  examples
  describe "From solutions" $ do
    properties Solution.paths "paths"
