{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{- |
Description: Graceful tree labeling
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Some solutions to "Problems.P92" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P92 (gracefulTree, gracefulTree') where

import           Control.Applicative ((<|>))
import           Data.List           (permutations, sort, sortOn)
import           Data.Map.Lazy       (Map, (!))
import qualified Data.Map.Lazy       as Map
import           Data.Maybe          (fromJust)
import           Data.Ord            (Down (..))
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Problems.Graphs
import           Problems.P81

{- | Gracefully label a tree graph.

This implementation builds up a partial graceful labeling, adding one vertex at a time.
It gives up and tries another if a partial labeling cannot be extended.
-}
gracefulTree :: G -> Maybe (Map Vertex Int)
gracefulTree g
  | Set.null $ vertexes g = Just Map.empty
  | isTree g              = expand p
  | otherwise             = undefined
  where vs = vertexesByDegree g
        p = Partial { graph = g
                    , labeling = Map.empty
                    , remainingVertexes = vs
                    , remainingVertexLabels = Set.fromList [1..(length vs)]
                    , definiteEdgeDifferences = Set.empty
                    }

-- | A partial graceful labeling of a tree graph and associate state for building it up.
data Partial = Partial
  { graph                   :: G               -- ^ Tree to label gracefully
  , labeling                :: Map Vertex Int  -- ^ Partial labeling of vertexes
  , remainingVertexes       :: [Vertex]        -- ^ Vertexes remaining to be labeled
  , remainingVertexLabels   :: Set Int         -- ^ Set of labels yet to be used with vertexes
  , definiteEdgeDifferences :: Set Int         -- ^ Set of differences already determined for edges
  }

-- | Trying vertexes with higher degrees first might constrain labeling more,
-- i.e, prune the search space more.
vertexesByDegree :: G -> [Vertex]
vertexesByDegree g = sortOn (\v -> Down $ Set.size $ neighbors v g) $ Set.toList $ vertexes g

-- | Label one more vertex.
expand :: Partial -> Maybe (Map Vertex Int)
expand Partial{ labeling = lbls, remainingVertexes = [] } = Just lbls
expand p = labelVertex p' v ls
  where (v:vs) = remainingVertexes p
        ls = remainingVertexLabels p
        p' = p { remainingVertexes = vs }

-- | Try labeling a particular vertex with the given label candidates.
labelVertex :: Partial -> Vertex -> Set Int -> Maybe (Map Vertex Int)
labelVertex p v ls | Set.null ls = Nothing
                   | disjoint    = expand p' <|> next
                   | otherwise   = next
  where l = Set.findMin ls  -- choose label to try arbitrarily
        e = edgeDiffs p v l
        es = definiteEdgeDifferences p
        disjoint = maybe False (Set.disjoint es) e
        next = labelVertex p v $ Set.delete l ls
        p' = p { labeling = Map.insert v l $ labeling p
               , remainingVertexLabels = Set.delete l $ remainingVertexLabels p
               , definiteEdgeDifferences = Set.union es $ fromJust e
               }

-- | Gather the absolute differences between vertexes in the edge connected to the given vertex.
-- The given vertex would not have been labeled, so this cannot be double-counted.
edgeDiffs :: Partial -> Vertex -> Int -> Maybe (Set Int)
edgeDiffs p v l | bijected  = Just diffs
                | otherwise = Nothing
  where ls = labeling p
        vs = Set.filter (`Map.member` ls) $ neighbors v $ graph p
        diffs = Set.map (\v' -> abs $ l - ls ! v') vs
        bijected = Set.size diffs == Set.size vs

isTree :: G -> Bool
isTree g = all (\(v,v') -> length (paths v v' g) == 1) [ (v,v') | v <- vs, v' <- vs, v < v' ]
  where vs = Set.toList $ vertexes g

{- | Gracefully label a tree graph.

This implementation tries all permutations of vertex labels
and checks if any are a graceful labeling.
-}
gracefulTree' :: G -> Maybe (Map Vertex Int)
gracefulTree' g
  | (v:_) <- labelings = Just v
  | otherwise          = Nothing
  where vs = Set.toList $ vertexes g
        candidates = map (Map.fromList . flip zip [1..]) $ permutations vs
        labelings = filter (isGracefulLabeling g) candidates

isGracefulLabeling :: G -> Map Vertex Int -> Bool
isGracefulLabeling g ls = diffs == lbls
  where diff (Edge (u,v)) = abs $ (ls ! u) - (ls ! v)
        diffs = sort $ map diff $ Set.toList $ edges g
        lbls = [1..(Set.size (vertexes g) - 1)]
