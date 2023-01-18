{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P14Spec (spec) where

import qualified Problems.P14          as Problem
import qualified Solutions.P14         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> [Int]) -> String -> Spec
properties dupli name = describe name $ do
  prop "duplicates nothing" $
    dupli [] `shouldBe` []

  prop "duplicates element" $ \xs x ys ->
    dupli (xs ++ [x] ++ ys) `shouldBe` dupli xs ++ [x,x] ++ dupli ys

examples :: Spec
examples = describe "Examples" $ do
  it "dupli [1, 2, 3]" $ do
    dupli [1, 2, 3] `shouldBe` [1,1,2,2,3,3 :: Int]

  where dupli = Problem.dupli

spec :: Spec
spec = parallel $ do
  properties Problem.dupli "dupli"
  examples
  describe "From solutions" $ do
    properties Solution.dupli "dupli"
