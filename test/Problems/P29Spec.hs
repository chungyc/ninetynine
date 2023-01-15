{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P29Spec (spec) where

import qualified Problems.P29          as Problem
import qualified Solutions.P29         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Integer) -> String -> Spec
properties fibonacci name = describe name $ do
  prop "is one for first Fibonacci number" $
    fibonacci 1 `shouldBe` 1

  prop "is one for second Fibonacci number" $
    fibonacci 2 `shouldBe` 1

  prop "is sum of two previous Fibonacci numbers" $ \(Positive n) ->
    n > 2 ==>
    fibonacci n `shouldBe` fibonacci (n-1) + fibonacci (n-2)

examples :: Spec
examples = describe "Examples" $ do
  it "map fibonacci [1..10]" $ do
    map fibonacci [1..10] `shouldBe` [1,1,2,3,5,8,13,21,34,55]

  where fibonacci n = Problem.fibonacci (n :: Int)

spec :: Spec
spec = parallel $ do
  properties Problem.fibonacci "fibonacci"
  examples
  describe "From solutions" $ do
    properties Solution.fibonacci "fibonacci"
