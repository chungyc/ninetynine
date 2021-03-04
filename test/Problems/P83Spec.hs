module Problems.P83Spec (spec) where

import           Control.Monad             (liftM)
import           Data.Maybe                (fromJust)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary
import           Problems.P81
import qualified Problems.P83              as Problem
import qualified Solutions.P83             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (G -> [G], G -> Bool, G -> Bool) -> (String, String, String) -> Spec
properties
  (spanningTrees, isTree, isConnected)
  (nameSpanningTrees, nameIsTree, nameIsConnected) = modifyMaxSize (const 25) $ do
  describe nameSpanningTrees $ do
    prop "includes only spanning trees" $
      withGraph $ \g ->
        classify (isConnectedGraph g) "connected" $
        mapM_ (\t -> t `shouldSatisfy` isSpanningTree g) (spanningTrees g)

    modifyMaxSize (\n -> ceiling $ (sqrt $ fromIntegral n :: Float)) $
      prop "includes a spanning tree" $
        forAll genPureTree $ \t ->
          let spanningTree = toSpanningTree t
              vs = vertexes spanningTree
              vs' = Set.toList vs
              pairs = [Edge (u, v) | u <- vs', v <- vs', u < v]
          in forAll (sublistOf pairs) $ \es' ->
            let es = Set.union (edges spanningTree) $ Set.fromList es'
                g = fromJust $ toGraph (vs, es)
            in classify (Set.size (vertexes g) <= 1) "trivial" $
               (spanningTree, spanningTrees g) `shouldSatisfy` (\(g', gs) -> g' `elem` gs)

  describe nameIsTree $ do
    prop "if and only if tree" $
      withGraph $ \g ->
        classify (isTreeGraph g) "tree" $
        isTree g `shouldBe` isTreeGraph g

  describe nameIsConnected $ do
    prop "if and only if connected" $
      withGraph $ \g ->
        classify (isConnectedGraph g) "connected" $
        isConnected g `shouldBe` isConnectedGraph g

spec :: Spec
spec = parallel $ do
  properties
    (Problem.spanningTrees, Problem.isTree, Problem.isConnected)
    ("spanningTrees", "isTree", "isConnected")
  describe "From solutions" $ do
    properties
      (Solution.spanningTrees, Solution.isTree, Solution.isConnected)
      ("spanningTrees", "isTree", "isConnected")

-- | Whether second graph is a spanning tree of the first.
isSpanningTree :: G -> G -> Bool
isSpanningTree g g' = vertexes g == vertexes g' && isTreeGraph g'

isTreeGraph :: G -> Bool
isTreeGraph g = all (\(u, v) -> (length $ paths u v g) == 1) pairs
  where vs = Set.toList $ vertexes g
        pairs = [(u, v) | u <- vs, v <- vs, u < v]

isConnectedGraph :: G -> Bool
isConnectedGraph g = all (\(u, v) -> not $ null $ paths u v g) pairs
  where vs = Set.toList $ vertexes g
        pairs = [(u, v) | u <- vs, v <- vs, u < v]

toSpanningTree :: PureTree -> G
toSpanningTree t = fromJust $ toGraph (vs, es)
  where (_, vs, es) = toSpanningTree' t (1, Set.singleton 1, Set.empty)

toSpanningTree' :: PureTree -> (Vertex, Set Vertex, Set Edge) -> (Vertex, Set Vertex, Set Edge)
toSpanningTree' (Branch ts) r@(n, _, _) = fromChildren n ts r

fromChildren :: Vertex -> [PureTree] -> (Vertex, Set Vertex, Set Edge) -> (Vertex, Set Vertex, Set Edge)
fromChildren _ [] r = r
fromChildren v (t:ts) (n, vs, es) = fromChildren v ts $ toSpanningTree' t (n', vs', es')
  where n' = n+1
        vs' = Set.insert n' vs
        es' = Set.insert (Edge (v, n')) es

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
