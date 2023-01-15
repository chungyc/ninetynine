{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P15Spec (spec) where

import qualified Problems.P15          as Problem
import qualified Solutions.P15         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Int -> [Int]) -> String -> Spec
properties repli name = describe name $ do
  prop "repeats nothing" $ \(NonNegative k) ->
    repli [] k `shouldBe` []

  prop "repeats element" $ \xs -> \x -> \ys -> \(NonNegative k) ->
    repli (xs ++ [x] ++ ys) k `shouldBe` repli xs k ++ replicate k x ++ repli ys k

examples :: Spec
examples = describe "Examples" $ do
  it "repli \"abc\" 3" $ do
    repli "abc" 3 `shouldBe` "aaabbbccc"

  where repli = Problem.repli

spec :: Spec
spec = parallel $ do
  properties Problem.repli "repli"
  examples
  describe "From solutions" $ do
    properties Solution.repli "repli"
