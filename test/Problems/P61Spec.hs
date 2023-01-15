{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P61Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import           Problems.P54
import qualified Problems.P61                   as Problem
import qualified Solutions.P61                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Tree Int -> [Int], Tree Int -> [Int]) -> (String, String) -> Spec
properties (leaves, internals) (leavesName, internalsName) = do
  describe leavesName $ do
    prop "empty tree has no leaves" $ do
      leaves Empty `shouldBe` []

    prop "has leaf" $ \x ->
      leaves (Branch x Empty Empty) `shouldBe` [x]

    prop "has leaves from subtrees" $ \t -> \t' -> \x ->
      t /= Empty || t' /= Empty ==>
      leaves (Branch x t t') `shouldMatchList` leaves t ++ leaves t'

  describe internalsName $ do
    prop "empty tree has no internal node" $
      internals Empty `shouldBe` []

    prop "leaf has no internal node" $
      \x -> internals (Branch x Empty Empty) `shouldBe` []

    prop "internal nodes are itself and its subtrees" $ \t -> \t' -> \x ->
      t /= Empty || t' /= Empty ==>
      internals (Branch x t t') `shouldMatchList` [x] ++ internals t ++ internals t'

examples :: Spec
examples = describe "Examples" $ do
  it "leaves tree4" $ do
    leaves tree4 `shouldMatchList` [4,2]

  it "internals tree4" $ do
    internals tree4 `shouldMatchList` [2,1]

  where leaves = Problem.leaves
        internals = Problem.internals

spec :: Spec
spec = parallel $ do
  properties (Problem.leaves, Problem.internals) ("leaves", "internals")
  examples
  describe "From solutions" $ do
    properties (Solution.leaves, Solution.internals) ("leaves", "internals")
