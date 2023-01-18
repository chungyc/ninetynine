{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P89Spec (spec) where

import           Data.Maybe                (fromJust)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary ()
import           Problems.P80
import qualified Problems.P89              as Problem
import qualified Solutions.P89             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (G -> Bool) -> String -> Spec
properties bipartite name = describe name $ do
  prop "is true for bipartite graphs" $ \(Positive n) ->
    forAll (chooseInt (1,n-1)) $ \m ->
    forAll (sublistOf [Edge (u,v) | u <- [1..m], v <- [m+1..n]]) $ \es ->
    let g = fromJust $ toGraph (Set.fromList [1..n], Set.fromList es)
    in counterexample (show g) $
       bipartite g `shouldBe` True

  modifyMaxSize (const 20) $ do
    prop "is true if and only if graph is bipartite" $ \g -> bipartite g
      `shouldBe` isBipartite g

examples :: Spec
examples = describe "Examples" $ do
  it "bipartite $ toG $ Paths [[1,2,3,4],[1,4,5,2]]" $ do
    bipartite (toG $ Paths [[1,2,3,4], [1,4,5,2]]) `shouldBe` True

  it "bipartite $ toG $ Paths [[1,2,3,4],[1,4,5,2],[1,3]]" $ do
    bipartite (toG $ Paths [[1,2,3,4],[1,4,5,2],[1,3]]) `shouldBe` False

  where bipartite = Problem.bipartite

spec :: Spec
spec = parallel $ do
  properties Problem.bipartite "bipartite"
  examples
  describe "From solutions" $ do
    properties Solution.bipartite "bipartite"

-- | Straightforward brute force test of bipartiteness from definition.
isBipartite :: G -> Bool
isBipartite g = any (isBipartiteMatch g) $ Set.powerSet $ vertexes g

-- | Whether the vertex set vs and its complement is a bipartite matching.
isBipartiteMatch :: G -> Set Vertex -> Bool
isBipartiteMatch g vs = not (hasInternalEdge g vs || hasInternalEdge g vs')
  where vs' = Set.difference (vertexes g) vs

-- | Whether the vertex set vs has any edges between the vertexes in the set.
hasInternalEdge :: G -> Set Vertex -> Bool
hasInternalEdge g vs = not $ Set.disjoint pairs $ edges g
  where pairs = Set.fromList [Edge (u,v) | u <- Set.toList vs, v <- Set.toList vs, u < v]
