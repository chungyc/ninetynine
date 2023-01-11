{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P62Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import           Problems.P54
import qualified Problems.P62                   as Problem
import qualified Solutions.P62                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Tree Int -> Int -> [Int]) -> String -> Spec
properties atLevel name = describe name $ do
  prop "root node is at level 1" $ \x -> \t -> \t' ->
    atLevel (Branch x t t') 1 `shouldBe` [x]

  prop "empty when level is too deep" $ \t -> \(Small level) ->
    treeHeight t < level ==>
    atLevel t level `shouldBe` []

  prop "subtrees have one more level" $ \x -> \t -> \t' -> \(Small level) ->
    level > 1 ==>
    max (treeHeight t) (treeHeight t') <= level ==>
    atLevel (Branch x t t') level `shouldMatchList`
    atLevel t (level-1) ++ atLevel t' (level-1)

examples :: Spec
examples = describe "Examples" $ do
  it "atLevel tree4 2" $ do
    atLevel tree4 2 `shouldBe` [2,2]

  where atLevel = Problem.atLevel

spec :: Spec
spec = parallel $ do
  properties Problem.atLevel "atLevel"
  examples
  describe "From solutions" $ do
    properties Solution.atLevel "atLevel"
