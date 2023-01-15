{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P31Spec (spec) where

import qualified Problems.P31          as Problem
import qualified Solutions.P31         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Bool) -> String -> Spec
properties isPrime name = describe name $ do
  prop "if and only if there are exactly two divisors" $ \(Positive n) ->
    isPrime n `shouldBe` [1, n] == filter (\k -> n `mod` k == 0) [1..n]

examples :: Spec
examples = describe "Examples" $ do
  it "isPrime 7" $ do
    isPrime 7 `shouldBe` True

  it "isPrime 15" $ do
    isPrime 15 `shouldBe` False

  where isPrime n = Problem.isPrime (n :: Int)

spec :: Spec
spec = parallel $ do
  properties Problem.isPrime "isPrime"
  examples
  describe "From solutions" $ do
    properties Solution.isPrime   "isPrime"
    properties Solution.isPrime'  "isPrime'"
    properties Solution.isPrime'' "isPrime''"
