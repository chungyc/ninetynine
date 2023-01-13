{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P92Spec (spec) where

import           Data.List                 (sort)
import           Data.Map                  (Map, (!))
import           Data.Maybe                (fromJust)
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary ()
import qualified Problems.P92              as Problem
import qualified Solutions.P92             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (G -> Maybe (Map Vertex Int)) -> String -> Int -> Spec
properties gracefulTree name size = describe name $
  modifyMaxSize (const size) $ do

  prop "is graceful labeling" $ \(TreeGraph g) ->
    gracefulTree g `shouldSatisfy` isGracefulLabeling g

examples :: Spec
examples = describe "Examples" $ do
  it "gracefulTree tree92" $ do
    gracefulTree tree92 `shouldSatisfy` isGracefulLabeling tree92

  it "gracefulTree tree92'" $ do
    gracefulTree tree92' `shouldSatisfy` isGracefulLabeling tree92'

  where gracefulTree = Problem.gracefulTree
        tree92 = Problem.tree92
        tree92' = Problem.tree92'

spec :: Spec
spec = parallel $ do
  properties Problem.gracefulTree "gracefulTree" 15
  examples
  describe "From solutions" $ do
    properties Solution.gracefulTree  "gracefulTree" 15
    properties Solution.gracefulTree' "gracefulTree'" 10

isGracefulLabeling :: G -> Maybe (Map Vertex Int) -> Bool
isGracefulLabeling _ Nothing   = False
isGracefulLabeling g (Just ls) = diffs == lbls
  where diff (Edge (u,v)) = abs $ (ls ! u) - (ls ! v)
        diffs = sort $ map diff $ Set.toList $ edges g
        lbls = [1..(Set.size (vertexes g) - 1)]

-- | Generates graphs which are trees.
trees :: Gen G
trees = sized gen
  where gen 0 = single
        gen _ = frequency [ (1, single), (10, extended) ]

        single = singletonGraph <$> arbitrary
          where singletonGraph v = fromJust $ toGraph (Set.singleton v, Set.empty)

        extended = scale (subtract 1) $ do
          t <- trees
          let vs = vertexes t
          let es = edges t
          v <- elements $ Set.toList vs
          v' <- arbitrary `suchThat` (\x -> not $ Set.member x vs)

          -- If a vertex not in a tree and an edge from a vertex in the tree
          -- to this vertex is added, then the new graph is also a tree since
          -- there can be only one path to any other vertex from the new vertex.
          let vs' = Set.insert v' vs
          let es' = Set.insert (Edge (v, v')) es
          return $ fromJust $ toGraph (vs', es')

-- | Shrink a graph which is a tree so that it is still a tree.
shrinkTree :: G -> [G]
shrinkTree g =
  [ fromJust $ toGraph (removeVertex v, removeEdgesWith v) | v <- leaves ]
  where leaves = filter (\v -> Set.size (neighbors v g) == 1) $ Set.toList $ vertexes g
        removeVertex v = Set.delete v $ vertexes g
        removeEdgesWith v = Set.filter (\(Edge (x,y)) -> x /= v && y /= v) $ edges g

-- | A tree graph.
newtype TreeGraph = TreeGraph G deriving Show

instance Arbitrary TreeGraph where
  arbitrary = TreeGraph <$> trees
  shrink (TreeGraph g) = map TreeGraph $ shrinkTree g
