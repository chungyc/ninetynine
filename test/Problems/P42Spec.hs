{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P42Spec (spec) where

import           Data.Maybe            (fromJust)
import           Problems.P33
import qualified Problems.P42          as Problem
import qualified Solutions.P42         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Integer -> Maybe Integer) -> String -> Spec
properties multiplicativeInverse name = describe name $ do
  prop "is multiplicative inverse" $
    \(Positive a) -> \(Positive n) -> n > 1 ==> coprime a n ==>
    multiplicativeInverse a n `shouldSatisfy` \x -> a * (fromJust x) `mod` n == 1

  prop "has no multiplicative inverse when not coprime" $
    \(Positive a) -> \(Positive n) -> n > 1 ==> not (coprime a n) ==>
    multiplicativeInverse a n `shouldBe` Nothing

examples :: Spec
examples = describe "Examples" $ do
  it "multiplicativeInverse 3 5" $
    multiplicativeInverse 3 5 `shouldBe` Just 2

  it "multiplicativeInverse 48 127" $ do
    multiplicativeInverse 48 127 `shouldBe` Just 45

  it "multiplicativeInverse 824 93" $ do
    multiplicativeInverse 824 93 `shouldBe` Just 50

  it "multiplicativeInverse 48 93" $ do
    multiplicativeInverse 48 93 `shouldBe` Nothing

  where multiplicativeInverse n = Problem.multiplicativeInverse (n :: Integer)

spec :: Spec
spec = parallel $ do
  properties Problem.multiplicativeInverse "multiplicativeInverse"
  examples
  describe "From solutions" $ do
    properties Solution.multiplicativeInverse "multiplicativeInverse"
