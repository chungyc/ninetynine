{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P92Spec (spec) where

import           Data.List                  (sort)
import           Data.Map                   (Map, (!))
import qualified Data.Set                   as Set
import           Problems.Graphs
import           Problems.Graphs.QuickCheck
import qualified Problems.P92               as Problem
import qualified Solutions.P92              as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: (G -> Maybe (Map Vertex Int)) -> String -> Int -> Spec
properties gracefulTree name size = describe name $ do
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
