{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P85Spec (spec) where

import           Data.List                 (permutations)
import           Data.Map                  (Map, (!))
import qualified Data.Map.Lazy             as Map
import           Data.Maybe                (fromJust)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary ()
import           Problems.P80
import qualified Problems.P85              as Problem
import qualified Solutions.P85             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (G -> G -> Bool) -> String -> Spec
properties isomorphic name = describe name $ do
  modifyMaxSize (const 15) $ do
    prop "is true for identical graphs" $ \g ->
      isomorphic g g `shouldBe` True

    prop "is true for permuted graphs" $ \g ->
      forAll (shuffle $ Set.toAscList $ vertexes g) $ \p ->
      let g' = permute g p
      in counterexample (show (g, g')) $
         isomorphic g g' `shouldBe` True

    prop "is not true for non-isomorphic graphs" $ \g ->
      not (Set.null $ vertexes g) ==>
      let v = Set.findMin (vertexes g)
          v' = Set.findMax (vertexes g) + 1
          vs = Set.insert v' $ vertexes g
          es = Set.insert (Edge (v,v')) $ edges g
          g' = fromJust $ toGraph (vs, es) :: G
      in forAll (shuffle $ Set.toAscList $ vertexes g') $ \p ->
        let g'' = permute g' p
        in counterexample (show (g, g'')) $
           isomorphic g g'' `shouldBe` False

    prop "is true if and only if graphs are isomorphic" $ \g g' ->
      isomorphic g g' `shouldBe` isomorphicByDefinition g g'

examples :: Spec
examples = do
  describe "Examples" $ do
    it "isomorphic (toG $ Paths [[1,2,3], [2,4]]) (toG $ Paths [[1,2,3],[1,4]])" $ do
      isomorphic (toG $ Paths [[1,2,3], [2,4]]) (toG $ Paths [[1,2,3],[1,4]]) `shouldBe` False

    it "isomorphic graph85 graph85'" $ do
      isomorphic graph85 graph85' `shouldBe` True

  where isomorphic = Problem.isomorphic
        graph85 = Problem.graph85
        graph85' = Problem.graph85'

spec :: Spec
spec = parallel $ do
  properties Problem.isomorphic "isomorphic"
  examples
  describe "From solutions" $ do
    properties Solution.isomorphic   "isomorphic"
    properties Solution.isomorphic'  "isomorphic'"
    properties Solution.isomorphic'' "isomorphic''"

permute :: G -> [Vertex] -> G
permute g p = fromJust $ toGraph (vs', es')
  where translate = Map.fromList $ zip (Set.toAscList $ vertexes g) p
        vs' = mapVertexes translate $ vertexes g
        es' = mapEdges translate $ edges g

mapVertexes :: Map Vertex Vertex -> Set Vertex -> Set Vertex
mapVertexes translate = Set.map (translate !)

mapEdges :: Map Vertex Vertex -> Set Edge -> Set Edge
mapEdges translate = Set.map (\(Edge (u, v)) -> Edge (f u, f v))
  where f = (!) translate

isomorphicByDefinition :: G -> G -> Bool
isomorphicByDefinition g g'
  | length vs == length vs' = any equivalentEdges bijections
  | otherwise = False
  where vs = Set.toList $ vertexes g
        vs' = Set.toList $ vertexes g'
        equivalentEdges f = and [ edgeContainedBy (x,y) g == edgeContainedBy (x',y') g'
                                | x <- vs, let x' = f x, y <- vs', let y' = f y]
        edgeContainedBy (x,y) z = Set.member (Edge (x,y)) $ edges z
        bijections = [ (m !) | m <- mappings ]
        mappings = [ Map.fromList $ zip vs ys | ys <- permutations vs' ]
