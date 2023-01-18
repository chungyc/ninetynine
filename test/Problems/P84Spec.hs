{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P84Spec (spec) where

import           Data.Map.Lazy             (Map)
import qualified Data.Map.Lazy             as Map
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary ()
import           Problems.P83
import qualified Problems.P84              as Problem
import qualified Solutions.P84             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (G -> Map Edge Int -> G) -> String -> Spec
properties minimumSpanningTree name = describe name $ do
  modifyMaxSize (const 5) $ do
    prop "is a spanning tree" $ \g (InfiniteList ws _) ->
      not (null $ vertexes g) ==>
      isConnected g ==>
      let weights = Map.fromList $ zip (Set.toList $ edges g) ws
      in minimumSpanningTree g weights `shouldSatisfy` flip elem (spanningTrees g)

    prop "has minimum weight sum for spanning trees" $ \g (InfiniteList ws _) ->
      not (null $ vertexes g) ==>
      isConnected g ==>
      let weights = Map.fromList $ zip (Set.toList $ edges g) ws
          weightSum g' = sum $ map (\e -> Map.findWithDefault 0 e weights) $ Set.toList $ edges g'
          minimumWeightSum = minimum $ map weightSum $ spanningTrees g
      in counterexample ("weighted graph: " ++ show (g, weights)) $
         counterexample ("spanning tree edges: " ++ show (map edges $ spanningTrees g)) $
         counterexample ("spanning tree weights: " ++ show (map weightSum $ spanningTrees g)) $
         weightSum (minimumSpanningTree g weights) `shouldBe` minimumWeightSum

examples :: Spec
examples = do
  describe "Examples" $ do
    it "minimumSpanningTree graph83 weights84" $
      let t = minimumSpanningTree graph83 weights84
      in sum (map (weights84 Map.!) $ Set.toList $ edges t) `shouldBe` 33

  where minimumSpanningTree = Problem.minimumSpanningTree
        weights84 = Problem.weights84

spec :: Spec
spec = parallel $ do
  properties Problem.minimumSpanningTree "minimumSpanningTree"
  examples
  describe "From solutions" $ do
    properties Solution.minimumSpanningTree "minimumSpanningTree"
