{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P83Spec (spec) where

import           Data.Maybe                (fromJust)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary ()
import           Problems.P80
import           Problems.P81
import           Problems.P83              (graph83)
import qualified Problems.P83              as Problem
import qualified Solutions.P83             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (G -> [G], G -> Bool, G -> Bool) -> (String, String, String) -> Spec
properties
  (spanningTrees, isTree, isConnected)
  (nameSpanningTrees, nameIsTree, nameIsConnected) = modifyMaxSize (const 16) $ do
  describe nameSpanningTrees $ do
    prop "includes only spanning trees" $ \g ->
      classify (isConnectedGraph g) "connected" $
      conjoin (map (\t -> t `shouldSatisfy` isSpanningTree g) $ spanningTrees g)

    prop "is idempotent" $ \g ->
      conjoin (map (\t -> t `shouldSatisfy` (==) [t] . spanningTrees) $ spanningTrees g)

    modifyMaxSize (const 9) $ do
      prop "includes arbitrary spanning tree" $
        forAll trees $ \t ->
        forAll (graphsSpannedBy t) $ \g ->
        counterexample (showGraph g) $
        spanningTrees (graph g) `shouldSatisfy` elem (graph t)

  describe nameIsTree $ do
    prop "if and only if tree" $ \g ->
      classify (isTreeGraph g) "tree" $
      isTree g `shouldBe` isTreeGraph g

  describe nameIsConnected $ do
    prop "if and only if connected" $ \g ->
      classify (isConnectedGraph g) "connected" $
      isConnected g `shouldBe` isConnectedGraph g

examples :: Spec
examples = do
  describe "Examples" $ do
    it "length $ spanningTrees graph83" $ do
      length (spanningTrees graph83) `shouldBe` 173

    it "(toG $ Paths [[1,2,4,5,6,7,8],[1,3],[5,10],[7,9]]) `elem` (spanningTrees graph83)" $ do
      spanningTrees graph83 `shouldSatisfy` elem (toG $ Paths [[1,2,4,5,6,7,8],[1,3],[5,10],[7,9]])

    it "isTree graph83" $ do
      isTree graph83 `shouldBe` False

    it "isTree $ toG $ Paths [[1,2,3],[1,4,5]]" $ do
      (isTree $ toG $ Paths [[1,2,3],[1,4,5]]) `shouldBe` True

    it "isConnected graph83" $ do
      isConnected graph83 `shouldBe` True

    it "isConnected $ toG $ Lists ([1,2,3], [])" $ do
      (isConnected $ toG $ Lists ([1,2,3], [])) `shouldBe` False

    where spanningTrees = Problem.spanningTrees
          isTree = Problem.isTree
          isConnected = Problem.isConnected

spec :: Spec
spec = parallel $ do
  properties
    (Problem.spanningTrees, Problem.isTree, Problem.isConnected)
    ("spanningTrees", "isTree", "isConnected")

  examples

  describe "From solutions" $ do
    properties
      (Solution.spanningTrees, Solution.isTree, Solution.isConnected)
      ("spanningTrees", "isTree", "isConnected")

-- | Conveniently create a graph representation from the vertexes and edges.
graph :: (Set Vertex, Set Edge) -> G
graph = fromJust . toGraph

-- | Use the paths representation to show graphs.
showGraph :: (Set Vertex, Set Edge) -> String
showGraph g = show (fromJust $ toGraph g :: Paths)

-- | Whether second graph is a spanning tree of the first.
isSpanningTree :: G -> G -> Bool
isSpanningTree g g' = vertexes g == vertexes g' && isTreeGraph g'

-- | Checking whether a graph is a tree in the most straightforward way.
-- I.e., that there is exactly one path between any two vertexes.
isTreeGraph :: G -> Bool
isTreeGraph g = all (\(u, v) -> (length $ paths u v g) == 1) pairs
  where vs = Set.toList $ vertexes g
        pairs = [(u, v) | u <- vs, v <- vs, u < v]

-- | Checking whether a graph is connected in the most straightforward way.
-- I.e., that there is some path between any two vertexes.
isConnectedGraph :: G -> Bool
isConnectedGraph g = all (\(u, v) -> not $ null $ paths u v g) pairs
  where vs = Set.toList $ vertexes g
        pairs = [(u, v) | u <- vs, v <- vs, u < v]

-- | Generates graphs which are trees.
trees :: Gen (Set Vertex, Set Edge)
trees = sized gen
  where gen 0 = single
        gen _ = frequency [ (1, single), (10, extended) ]

        single = (\v -> (Set.singleton v, Set.empty)) <$> arbitrary

        extended = scale (subtract 1) $ do
          (vs, es) <- trees
          v <- elements $ Set.toList vs
          v' <- arbitrary `suchThat` (\x -> not $ Set.member x vs)

          -- If a vertex not in a tree and an edge from a vertex in the tree
          -- to this vertex is added, then the new graph is also a tree since
          -- there can be only one path to any other vertex from the new vertex.
          let vs' = Set.insert v' vs
          let es' = Set.insert (Edge (v, v')) es
          return (vs', es')

-- | Generates graphs which are spanned by the given graph.
--
-- I.e., the set of vertexes are the same as the given graph,
-- and its set of edges includes at least those in the given graph.
graphsSpannedBy :: (Set Vertex, Set Edge) -> Gen (Set Vertex, Set Edge)
graphsSpannedBy (vs, es) = do
  let l = Set.toList vs
  es' <- sublistOf [ Edge (u, v) | u <- l, v <- l, u < v ]
  return (vs, Set.union es $ Set.fromList es')
