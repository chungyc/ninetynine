{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P72Spec (spec) where

import           Problems.MultiwayTrees
import           Problems.MultiwayTrees.Arbitrary ()
import qualified Problems.P72                     as Problem
import qualified Solutions.P72                    as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: (MultiwayTree Int -> [Int]) -> String -> Spec
properties postOrderSequence name = describe name $ do
  prop "is singleton sequence for singleton tree" $ \x ->
    postOrderSequence (MultiwayTree x []) `shouldBe` [x]

  prop "is post-order sequence" $ \x ts ->
    postOrderSequence (MultiwayTree x ts) `shouldBe`
    concatMap postOrderSequence ts ++ [x]

examples :: Spec
examples = describe "Examples" $ do
  it "postOrderSequence multitree5" $ do
    postOrderSequence multitree5 `shouldBe` "gfcdeba"

  where postOrderSequence = Problem.postOrderSequence

spec :: Spec
spec = parallel $ do
  properties Problem.postOrderSequence "postOrderSequence"
  examples
  describe "From solutions" $ do
    properties Solution.postOrderSequence "postOrderSequence"
