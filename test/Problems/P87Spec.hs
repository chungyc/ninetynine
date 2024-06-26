{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P87Spec (spec) where

import           Data.List                 (inits, nub, tails)
import           Data.Maybe                (fromJust)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Problems.Graphs
import           Problems.Graphs.Arbitrary ()
import           Problems.P80
import qualified Problems.P87              as Problem
import qualified Solutions.P87             as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (G -> Vertex -> [Vertex]) -> String -> Spec
properties depthFirst name = describe name $ do
  prop "starts from starting point" $ \g ->
    g /= empty ==>
    forAll (vertexesIn g) $ \v ->
    depthFirst g v `shouldSatisfy` (==) v . head

  prop "is not cyclic" $ \g ->
    g /= empty ==>
    forAll (vertexesIn g) $ \v ->
    depthFirst g v `shouldSatisfy` \l -> l == nub l

  prop "are reachable vertexes from starting point" $ \g ->
    g /= empty ==>
    forAll (vertexesIn g) $ \v ->
    depthFirst g v `shouldSatisfy`
    (==) (reachable g Set.empty (Set.singleton v) Set.empty) . Set.fromList

  prop "traverses depth first" $ \g ->
    g /= empty ==>
    forAll (vertexesIn g) $ \v ->
    depthFirst g v `shouldSatisfy` isDepthFirstSequence g

  where vertexesIn g = elements $ Set.toList $ vertexes g
        empty = fromJust $ toGraph (Set.empty, Set.empty)

examples :: Spec
examples = describe "Examples" $ do
  it "depthFirst (toG $ Paths [[1,2,3,4,5], [2,4], [6,7]]) 1" $ do
    depthFirst (toG $ Paths [[1,2,3,4,5], [2,4], [6,7]]) 1
      `shouldSatisfy` flip elem [[1,2,3,4,5], [1,2,4,5,3], [1,2,4,3,5]]

  where depthFirst = Problem.depthFirst

spec :: Spec
spec = parallel $ do
  properties Problem.depthFirst "depthFirst"
  examples
  describe "From solutions" $ do
    properties Solution.depthFirst "depthFirst"

isDepthFirstSequence :: G -> [Vertex] -> Bool
isDepthFirstSequence g xs = all check splits
  where splits = zip (inits xs) (tails xs)
        check (_, [])         = True
        check (front, v:back) = isDepthFirst g front v back

-- | Whether v in the sequence satisfies the depth-first property.
--
-- front ++ [v] ++ back is the sequence.
-- By definition, back must start with all vertexes reachable from v,
-- except for those which are only reachable through vertexes in front.
isDepthFirst :: G -> [Vertex] -> Vertex -> [Vertex] -> Bool
isDepthFirst g front v back = reached == Set.fromList backprefix
  where reached = Set.delete v $ reachable g vs (Set.singleton v) Set.empty
        backprefix = take (Set.size reached) back
        vs = Set.fromList front

-- | Returns set of vertexes reachable from those in vs,
-- except through paths going through vertexes in excluded.
reachable :: G -> Set Vertex -> Set Vertex -> Set Vertex -> Set Vertex
reachable g excluded vs visited
  | Set.null vs          = visited
  | Set.member v visited = reachable g excluded vs' visited
  | otherwise            = reachable g excluded vs' visited''
  where visited'' = reachable g excluded ns visited'
        v = Set.findMin vs
        visited' = Set.insert v visited
        ns = Set.difference (neighbors v g) excluded
        vs' = Set.delete v vs
