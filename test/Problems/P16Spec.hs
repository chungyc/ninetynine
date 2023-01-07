{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P16Spec (spec) where

import qualified Problems.P16          as Problem
import qualified Solutions.P16         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Int -> [Int]) -> String -> Spec
properties dropEvery name = describe name $ do
  prop "drops nothing" $
    \(Positive k) -> dropEvery [] k `shouldBe` []

  prop "drops every n element" $
    \(Positive n) -> \(Positive q) -> forAll (vectorOf (q*n-1) arbitrary) $ \xs -> \x ->
      dropEvery (xs ++ [x]) n `shouldBe` dropEvery xs n

  prop "does not drop other elements" $
    \(Positive n) -> \xs -> \x ->
      (length xs + 1) `mod` n /= 0 ==>
      dropEvery (xs ++ [x]) n `shouldBe` dropEvery xs n ++ [x]

examples :: Spec
examples = describe "Examples" $ do
  it "dropEvery \"abcdefghik\" 3" $ do
    dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

  where dropEvery = Problem.dropEvery

spec :: Spec
spec = parallel $ do
  properties Problem.dropEvery "dropEvery"
  examples
  describe "From solutions" $ do
    properties Solution.dropEvery "dropEvery"
