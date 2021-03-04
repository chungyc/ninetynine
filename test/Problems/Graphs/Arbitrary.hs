module Problems.Graphs.Arbitrary (Sets (Sets), withGraph, Arbitrary) where

import           Data.Maybe      (fromJust)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Problems.Graphs
import           Test.QuickCheck

-- | Helper type for generating sets of vertexes and edges for a valid graph.
--
-- Example:
--
-- > spec :: Spec
-- > spec = do
-- >   prop "sets composed with toGraph is identity" $
-- >     \(Sets (vs, es)) -> sets (fromJust $ toGraph' (vs, es)) `shouldBe` (vs, es)
newtype Sets = Sets (Set Vertex, Set Edge)
  deriving Show

instance Arbitrary Sets where
  arbitrary = scale (ceiling . scaledSize) $ do
    vs <- arbitrary
    es <- sublistOf [Edge (u, v) | u <- vs, v <- vs, u < v]
    return $ Sets (Set.fromList vs, Set.fromList es)
      -- For k vertexes, there can be about k^2 / 2 edges.
      where scaledSize n = (*) 2 $ sqrt $ fromIntegral n :: Float

-- | Helper function which takes in sets of vertexes and edges for a valid graph,
-- turns it into a graph, and applies the given function.
--
-- Useful for generating arbitrary valid graphs in property tests.
-- It is easy to generate sets of vertexes and edges for a valid graph,
-- but hard to generate graphs of multiple types directly.
-- Thus, the sets are arbitrarily generated, which are then turned into a graph.
--
-- Example:
--
-- > spec :: Spec
-- > spec = do
-- >   prop "sets are vertexes and edges" $
-- >     withGraph $ \g -> sets g `shouldBe` (vertexes g, edges g)
withGraph :: Graph g => (g -> a) -> Sets -> a
withGraph f (Sets s) = let g = fromJust $ toGraph s in f g
