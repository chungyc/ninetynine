{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P33Spec (spec) where

import qualified Problems.P33          as Problem
import qualified Solutions.P33         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Integer -> Bool) -> String -> Spec
properties coprime name = describe name $ do
  prop "if greatest common divisor is 1" $ \(Positive n) (Positive m) ->
    coprime n m `shouldBe` gcd n m == 1

examples :: Spec
examples = describe "Examples" $ do
  it "coprime 35 64" $ do
    coprime 35 64 `shouldBe` True

  where coprime a b = Problem.coprime a (b :: Int)

spec :: Spec
spec = parallel $ do
  properties Problem.coprime "coprime"
  examples
  describe "From solutions" $ do
    properties Solution.coprime "coprime"
