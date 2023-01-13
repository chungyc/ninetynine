{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.Graphs.QuickCheck
  ( Sets (..)
  , trees
  , shrinkTree
  , TreeGraph (..)
  ) where

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
    vs <- (\n -> [1..n]) <$> getSize
    es <- sublistOf [Edge (u, v) | u <- vs, v <- vs, u < v]
    return $ Sets (Set.fromList vs, Set.fromList es)
      -- For k vertexes, there can be about k^2 / 2 edges.
      where scaledSize n = (*) 2 $ sqrt $ fromIntegral n :: Float

  shrink (Sets (vs, es)) = map (\v -> Sets (Set.delete v vs, remove v)) $ Set.toList vs
    where remove v = Set.filter (\(Edge (u', v')) -> u' == v || v' == v) es

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

-- | An arbitrary graph which is a tree.
newtype TreeGraph = TreeGraph G deriving Show

instance Arbitrary TreeGraph where
  arbitrary = TreeGraph <$> trees
  shrink (TreeGraph g) = map TreeGraph $ shrinkTree g
