module Problems.P81Spec (spec) where

import           Data.List                 (nub)
import           Data.Maybe                (fromJust)
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary
import           Problems.P80
import qualified Problems.P81              as Problem
import qualified Solutions.P81             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Vertex -> Vertex -> G -> [[Vertex]]) -> String -> Spec
properties paths name =
  -- Cap size to limit combinatorial explosion
  describe name $ modifyMaxSize (const 40) $ do
    prop "includes path" $ do
      \(Sets (vs, es)) -> do
        vs' <- sublistOf $ Set.toList vs
        return $ forAll (shuffle vs') $ \trail ->
          length trail > 1 ==>
          head trail /= last trail ==>
          let path = nub trail
              a = head path
              b = last path
              es' = Set.union es $ pathEdges path  -- ensure path exists
          in paths a b (fromJust $ toGraph (vs, es')) `shouldSatisfy` elem path

    prop "does not include non-existant path" $ do
      \(Sets (vs, es)) -> do
        vs' <- sublistOf $ Set.toList vs
        return $ forAll (shuffle vs') $ \trail ->
          length trail > 1 ==>
          head trail /= last trail ==>
          let path = nub trail
              a = head path
              b = last path
              es' = Set.difference es $ pathEdges path  -- ensure path does not exist
          in paths a b (fromJust $ toGraph (vs, es')) `shouldNotSatisfy` elem path

  where pathEdges []             = Set.empty
        pathEdges [_]            = Set.empty
        pathEdges (u : vs@(v:_)) = Set.insert (Edge (u, v)) $ pathEdges vs

examples :: Spec
examples = do
  describe "Examples" $ do
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
