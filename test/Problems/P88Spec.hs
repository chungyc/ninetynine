{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P88Spec (spec) where

import           Data.List                 (sort)
import           Data.Maybe                (fromJust)
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary ()
import           Problems.P80
import           Problems.P87
import qualified Problems.P88              as Problem
import qualified Solutions.P88             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (G -> [[Vertex]]) -> String -> Spec
properties connectedComponents name = describe name $ do
  prop "has paths between connected components" $
    \g -> connectedComponents g
    `shouldSatisfy` all (\vs -> all (isConnected g) $ pairs vs)

  prop "does not have paths between unconnected components" $
    \(Positive n) -> \(Positive m) ->
      forAll (sublistOf [Edge (u,v) | u <- [1..n], v <- [u+1..n]]) $ \es ->
      forAll (sublistOf [Edge (u,v) | u <- [n+1..n+m], v <- [u+1..n+m]]) $ \es' ->
      let g = fromJust $ toGraph (Set.fromList [1..n+m], Set.fromList $ es ++ es')
      in counterexample (show g) $
         connectedComponents g `shouldSatisfy` all (not . isConnected g) . pairs . map head
         -- connectivity is transitive; checks against from only one vertex from each component

  where isConnected g (u,v) = elem v $ depthFirst g u
        pairs vs = [(u,v) | u <- vs, v <- vs, u < v]

examples :: Spec
examples = describe "Examples" $ do
  it "sort $ map sort $ connectedComponents $ toG $ Paths [[1,2,3,4,5], [2,4], [6,7]]" $ do
    map sort (connectedComponents $ toG $ Paths [[1,2,3,4,5], [2,4], [6,7]])
      `shouldMatchList` [[1,2,3,4,5], [6,7]]

  where connectedComponents = Problem.connectedComponents

spec :: Spec
spec = parallel $ do
  properties Problem.connectedComponents "connectedComponents"
  examples
  describe "From solutions" $ do
    properties Solution.connectedComponents "connectedComponents"
