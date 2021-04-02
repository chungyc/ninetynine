{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P92Spec (spec) where

import           Control.Monad
import           Data.List                 (sort)
import           Data.Map                  (Map, (!))
import           Data.Maybe                (fromJust)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary ()
import           Problems.P81
import qualified Problems.P92              as Problem
import qualified Solutions.P92             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (G -> Maybe (Map Vertex Int)) -> String -> Int -> Spec
properties gracefulTree name size= do
  modifyMaxSize (const size) $ do
    describe name $ do
      prop "is graceful labeling" $
        withTreeGraph $ \g -> isTree g ==>
                              gracefulTree g `shouldSatisfy` isGracefulLabeling g

  where withTreeGraph f = forAll genPureTree $ \t -> let g = toTree t in f g

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
  properties Problem.gracefulTree "gracefulTree" 20
  examples
  describe "From solutions" $ do
    properties Solution.gracefulTree  "gracefulTree" 20
    properties Solution.gracefulTree' "gracefulTree'" 10

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

-- | Create vertexes and edges from a structure-only tree.
toTree :: PureTree -> G
toTree t = fromJust $ toGraph (vs, es)
  where (_, vs, es) = toTree' t (1, Set.singleton 1, Set.empty)

toTree' :: PureTree -> (Vertex, Set Vertex, Set Edge) -> (Vertex, Set Vertex, Set Edge)
toTree' (Branch ts) r@(n, _, _) = fromChildren n ts r

fromChildren :: Vertex -> [PureTree] -> (Vertex, Set Vertex, Set Edge) -> (Vertex, Set Vertex, Set Edge)
fromChildren _ [] r = r
fromChildren v (t:ts) (n, vs, es) = fromChildren v ts $ toTree' t (n', vs', es')
  where n' = n+1
        vs' = Set.insert n' vs
        es' = Set.insert (Edge (v, n')) es

-- | Generate a tree that is pure structure.
-- This will be turned into a graph that is a tree.
data PureTree = Branch [PureTree]
  deriving Show

genPureTree :: Gen PureTree
genPureTree = sized genPureTree'

genPureTree' :: Int -> Gen PureTree
genPureTree' 0 = return $ Branch []
genPureTree' n
  | n > 0     = do
      k <- choose (0, n-1)
      liftM Branch $ vectorOf k $ genPureTree' $ (n-1) `div` (max 1 k)
  | otherwise = undefined
