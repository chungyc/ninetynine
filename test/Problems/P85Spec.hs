module Problems.P85Spec (spec) where

import           Data.Map                  (Map, (!))
import qualified Data.Map.Lazy             as Map
import           Data.Maybe                (fromJust)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary
import qualified Problems.P85              as Problem
import qualified Solutions.P85             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (G -> G -> Bool) -> String -> Spec
properties isomorphic name = modifyMaxSize (const 25) $ do
  describe name $ do
    prop "is true for permuted graphs" $
      withGraph $ \g -> forAll (shuffle $ Set.toAscList $ vertexes g) $ \p ->
      let g' = permute g p
      in counterexample (show (g, g')) $
         isomorphic g g' `shouldBe` True

    prop "is not true for non-isomorphic graphs" $
      withGraph $ \g -> not (Set.null $ vertexes g) ==>
      let v = Set.findMin (vertexes g)
          v' = Set.findMax (vertexes g) + 1
          vs = Set.insert v' $ vertexes g
          es = Set.insert (Edge (v,v')) $ edges g
          g' = fromJust $ toGraph (vs, es) :: G
      in forAll (shuffle $ Set.toAscList $ vertexes g') $ \p ->
        let g'' = permute g' p
        in counterexample (show (g, g'')) $
           isomorphic g g'' `shouldBe` False

spec :: Spec
spec = focus $ parallel $ do
  properties Problem.isomorphic "isomorphic"
  describe "From solutions" $ do
    properties Solution.isomorphic "isomorphic"

permute :: G -> [Vertex] -> G
permute g p = fromJust $ toGraph (vs', es')
  where translate = Map.fromList $ zip (Set.toAscList $ vertexes g) p
        vs' = mapVertexes translate $ vertexes g
        es' = mapEdges translate $ edges g

mapVertexes :: Map Vertex Vertex -> Set Vertex -> Set Vertex
mapVertexes translate vs = Set.map (translate !) vs

mapEdges :: Map Vertex Vertex -> Set Edge -> Set Edge
mapEdges translate es = Set.map (\(Edge (u, v)) -> Edge (f u, f v)) es
  where f = (!) translate
