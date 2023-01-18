{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.GraphsSpec (spec) where

import           Data.Maybe                 (fromJust)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Problems.Graphs
import           Problems.Graphs.QuickCheck
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "General graph properties" $ do
    general "Lists" (toGraph :: C Lists)
    general "Adjacency" (toGraph :: C Adjacency)
    general "Paths" (toGraph :: C Paths)
    general "G" (toGraph :: C G)

general :: (Graph g, Show g, Eq g) => String -> ((Set Vertex, Set Edge) -> Maybe g) -> Spec
general name toGraph' = do
  context ("with " ++ name) $ do
    prop "sets composed with toGraph is identity" $
      \(Sets (vs, es)) -> sets (fromJust $ toGraph' (vs, es)) `shouldBe` (vs, es)

    prop "edges are between vertexes" $
      withGraph $ \g ->
        let toSet (Edge (u, v)) = Set.fromList [u, v]
            vertexesFromEdges = Set.unions $ Set.map toSet $ edges g
        in vertexesFromEdges `shouldSatisfy` flip Set.isSubsetOf (vertexes g)

    prop "sets are vertexes and edges" $
      withGraph $ \g -> sets g `shouldBe` (vertexes g, edges g)

    prop "adjacent vertexes form an edge" $
      withGraph $ \g ->
        conjoin (map (\e@(u, v) -> adjacent u v g `shouldBe` Set.member (Edge e) (edges g)) (vertexPairs g))

    prop "adjacent vertexes are neighbors" $
      withGraph $ \g ->
        conjoin (map (\(u, v) -> adjacent u v g `shouldBe` Set.member u (neighbors v g)) (vertexPairs g))

    prop "neighbors are symmetric" $
      withGraph $ \g ->
        let isNeighborOf u v = Set.member u (neighbors v g)
        in conjoin (map (\(u, v) -> (u `isNeighborOf` v) `shouldBe` (v `isNeighborOf` u)) (vertexPairs g))

    prop "vertexes are symmetrically adjacent" $
      withGraph $ \g ->
        conjoin (map (\(u, v) -> adjacent u v g `shouldBe` adjacent v u g) (vertexPairs g))

    prop "graphs built are valid" $
      withGraph $ \g -> isValidGraph g

    prop "does not build graph with invalid sets of vertexes and edges" $
      \(Sets (_, es)) l ->
        let vs = Set.fromList l
            toSet (Edge (u, v)) = Set.fromList [u, v]
            doesNotInclude vs' e = not $ toSet e `Set.isSubsetOf` vs'
        in not (Set.null $ Set.filter (vs `doesNotInclude`) es) ==>
           toGraph' (vs, es) `shouldBe` Nothing

  where withGraph f (Sets s) = let g = fromJust $ toGraph' s in f g
        vertexPairs g = [(u,v) | let l = Set.toList $ vertexes g, u <- l, v <- l]

type C g = (Set Vertex, Set Edge) -> Maybe g
