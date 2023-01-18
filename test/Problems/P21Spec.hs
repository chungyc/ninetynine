{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P21Spec (spec) where

import qualified Problems.P21          as Problem
import qualified Solutions.P21         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: (Int -> [Int] -> Int -> [Int]) -> String -> Spec
properties insertAt name = describe name $ do
  prop "inserts at given position" $ \xs ys z ->
    insertAt z (xs ++ ys) (length xs + 1) `shouldBe` xs ++ [z] ++ ys

examples :: Spec
examples = describe "Examples" $ do
  it "insertAt 'X' \"abcd\" 2" $ do
    insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

  where insertAt = Problem.insertAt

spec :: Spec
spec = parallel $ do
  properties Problem.insertAt "insertAt"
  examples
  describe "From solutions" $ do
    properties Solution.insertAt "insertAt"
