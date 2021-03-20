module Problems.P92Spec (spec) where

import           Data.List                 (sort)
import           Data.Map                  (Map, (!))
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary
import           Problems.P81
import qualified Problems.P92              as Problem
import qualified Solutions.P92             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (G -> Maybe (Map Vertex Int)) -> String -> Spec
properties gracefulTree name = do
  modifyMaxSize (const 40) $ do
    describe name $ do
      prop "is graceful labeling" $
        withGraph $ \g -> isTree g ==>
                          gracefulTree g `shouldSatisfy` isGracefulLabeling g

examples :: Spec
examples = do
  describe "Examples" $ do
    it "gracefulTree tree92" $ do
      gracefulTree tree92 `shouldSatisfy` isGracefulLabeling tree92

    it "gracefulTree tree92'" $ do
      gracefulTree tree92' `shouldSatisfy` isGracefulLabeling tree92'

  where gracefulTree = Problem.gracefulTree
        tree92 = Problem.tree92
        tree92' = Problem.tree92'

spec :: Spec
spec = parallel $ do
  properties Problem.gracefulTree "gracefulTree"
  examples
  describe "From solutions" $ do
    properties Solution.gracefulTree  "gracefulTree"
    properties Solution.gracefulTree' "gracefulTree'"

isTree :: G -> Bool
isTree g = all (\(v,v') -> length (paths v v' g) == 1) pairs
  where vs = Set.toList $ vertexes g
        pairs = [(u,v) | u <- vs, v <- vs, u < v]

isGracefulLabeling :: G -> Maybe (Map Vertex Int) -> Bool
isGracefulLabeling _ Nothing   = False
isGracefulLabeling g (Just ls) = diffs == lbls
  where diff (Edge (u,v)) = abs $ (ls ! u) - (ls ! v)
        diffs = sort $ map diff $ Set.toList $ edges g
        lbls = [1..(Set.size (vertexes g) - 1)]
