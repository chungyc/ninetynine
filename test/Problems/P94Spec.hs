{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P94Spec (spec) where

import           Control.Monad         (forM_)
import qualified Data.Set              as Set
import           Problems.Graphs
import qualified Problems.P94          as Problem
import           Solutions.P85         (isomorphic)
import qualified Solutions.P94         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> Int -> [G]) -> String -> Spec
properties regularGraphs name = describe name $ do
  modifyMaxSize (const 6) $ do
    prop "have expected number of vertexes" $
      \(Positive n) -> forAll (chooseInt (1, n-1)) $ \k ->
        regularGraphs n k `shouldSatisfy` all ((==) n . Set.size . vertexes)

    prop "are k-regular graphs" $
      \(Positive n) -> forAll (chooseInt (1, n-1)) $ \k ->
        regularGraphs n k `shouldSatisfy`
        all (\g -> all (\v -> Set.size (neighbors v g) == k) $ vertexes g)

    prop "has non-isomorphic graphs" $
      \(Positive n) -> forAll (chooseInt (1, n-1)) $ \k ->
        regularGraphs n k `shouldSatisfy` \l ->
        all (not . uncurry isomorphic) [ (g, g') | g <- l, g' <- l, g /= g' ]

  -- It is too expensive for a trivial algorithm to compute
  -- the number of regular graphs for a test,
  -- so we compare with known numbers of regular graphs.
  describe "has expected number of graphs" $ parallel $ do
    forM_ [ ((3,2), 1)
          , ((4,2), 1)
          , ((4,3), 1)
          , ((5,2), 1)
          , ((5,3), 0)
          , ((5,4), 1)
          , ((6,2), 2)
          , ((6,3), 2)
          , ((6,4), 1)
          , ((6,5), 1)
          , ((7,2), 2)
          , ((7,3), 0)
          , ((7,4), 2)
          , ((7,5), 0)
          , ((7,6), 1)
          ] $ \((n, k), r) ->
      it ("with " ++ show (n,k)) $ do
        regularGraphs n k `shouldSatisfy` (==) r . length

examples :: Spec
examples = describe "Examples" $ do
  it "length $ regularGraphs 6 3" $ do
    length (regularGraphs 6 3) `shouldBe` 2

  where regularGraphs = Problem.regularGraphs

spec :: Spec
spec = parallel $ do
  properties Problem.regularGraphs "regularGraphs"
  examples
  describe "From solutions" $ do
    properties Solution.regularGraphs "regularGraphs"
