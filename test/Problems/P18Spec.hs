{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P18Spec (spec) where

import qualified Problems.P18          as Problem
import qualified Solutions.P18         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Int -> Int -> [Int]) -> String -> Spec
properties slice name = describe name $ do
  prop "extracts slice" $ \xs ys zs ->
    not (null ys) ==>
    slice (xs ++ ys ++ zs) (1 + length xs) (length xs + length ys) `shouldBe` ys

  prop "extracts slice when upper bound too large" $ \xs ys (Positive k) ->
    not (null ys) ==>
    slice (xs ++ ys) (1 + length xs) (k + length xs + length ys) `shouldBe` ys

  prop "extracts nothing when lower bound too large" $ \xs (Positive k) (NonNegative l) ->
      slice xs (k + length xs) (k + l + length xs) `shouldBe` []

examples :: Spec
examples = describe "Examples" $ do
  it "slice ['a','b','c','d','e','f','g','h','i','k'] 3 7" $ do
    slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"

  where slice = Problem.slice

spec :: Spec
spec = parallel $ do
  properties Problem.slice "slice"
  examples
  describe "From solutions" $ do
    properties Solution.slice  "slice"
    properties Solution.slice' "slice'"
