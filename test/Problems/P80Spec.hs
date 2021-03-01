module Problems.P80Spec (spec) where

import           Data.Maybe            (fromJust)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Problems.Graphs
import           Problems.P80
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "toLists" $ do
    prop "converts to same graph from Adjacency" $
      withGraph $ \g -> sets (toLists (g :: Adjacency)) `shouldBe` sets g

    prop "converts to same graph from Paths" $
      withGraph $ \g -> sets (toLists (g :: Paths)) `shouldBe` sets g

    prop "converts to same graph from G" $
      withGraph $ \g -> sets (toLists (g :: G)) `shouldBe` sets g

    prop "converts to valid graph from Adjacency" $
      withGraph $ \g -> toLists (g :: Adjacency) `shouldSatisfy` isValidGraph

    prop "converts to valid graph from Paths" $
      withGraph $ \g -> toLists (g :: Paths) `shouldSatisfy` isValidGraph

    prop "converts to valid graph from G" $
      withGraph $ \g -> toLists (g :: G) `shouldSatisfy` isValidGraph

  describe "toAdjacency" $ do
    prop "converts to same graph from Lists" $
      withGraph $ \g -> sets (toAdjacency (g :: Lists)) `shouldBe` sets g

    prop "converts to same graph from Paths" $
      withGraph $ \g -> sets (toAdjacency (g :: Paths)) `shouldBe` sets g

    prop "converts to same graph from G" $
      withGraph $ \g -> sets (toAdjacency (g :: G)) `shouldBe` sets g

    prop "converts to valid graph from Lists" $
      withGraph $ \g -> toAdjacency (g :: Lists) `shouldSatisfy` isValidGraph

    prop "converts to valid graph from Paths" $
      withGraph $ \g -> toAdjacency (g :: Paths) `shouldSatisfy` isValidGraph

    prop "converts to valid graph from G" $
      withGraph $ \g -> toAdjacency (g :: G) `shouldSatisfy` isValidGraph

  describe "toPaths" $ do
    prop "converts to same graph from Lists" $
      withGraph $ \g -> sets (toPaths (g :: Lists)) `shouldBe` sets g

    prop "converts to same graph from Adjacency" $
      withGraph $ \g -> sets (toPaths (g :: Adjacency)) `shouldBe` sets g

    prop "converts to same graph from G" $
      withGraph $ \g -> sets (toPaths (g :: G)) `shouldBe` sets g

    prop "converts to valid graph from Lists" $
      withGraph $ \g -> toPaths (g :: Lists) `shouldSatisfy` isValidGraph

    prop "converts to valid graph from Adjacency" $
      withGraph $ \g -> toPaths (g :: Adjacency) `shouldSatisfy` isValidGraph

    prop "converts to valid graph from G" $
      withGraph $ \g -> toPaths (g :: G) `shouldSatisfy` isValidGraph

  describe "toG" $ do
    prop "converts to same graph from Lists" $
      withGraph $ \g -> sets (toG (g :: Lists)) `shouldBe` sets g

    prop "converts to same graph from Adjacency" $
      withGraph $ \g -> sets (toG (g :: Adjacency)) `shouldBe` sets g

    prop "converts to same graph from Paths" $
      withGraph $ \g -> sets (toG (g :: Paths)) `shouldBe` sets g

    prop "converts to valid graph from Lists" $
      withGraph $ \g -> toG (g :: Lists) `shouldSatisfy` isValidGraph

    prop "converts to valid graph from Adjacency" $
      withGraph $ \g -> toG (g :: Adjacency) `shouldSatisfy` isValidGraph

    prop "converts to valid graph from Paths" $
      withGraph $ \g -> toG (g :: Paths) `shouldSatisfy` isValidGraph

  -- There are no separate tests here for "Solutions.P80".
  -- Their important bits are already tested by "GraphsSpec".

  where withGraph f (Sets s) = let g = fromJust $ toGraph s in f g

-- | Type for generating arbitrary sets of vertexes and edges which form a valid graph.
newtype Sets = Sets (Set Vertex, Set Edge)
  deriving Show

instance Arbitrary Sets where
  arbitrary = scale (ceiling . scaledSize) $ do
    vs <- arbitrary
    es <- sublistOf [Edge (u, v) | u <- vs, v <- vs, u <= v]
    return $ Sets (Set.fromList vs, Set.fromList es)
      -- For k vertexes, there can be about k^2 / 2 edges.
      where scaledSize n = (*) 2 $ sqrt $ fromIntegral n :: Float
