module Problems.P33Spec (spec) where

import qualified Problems.P33          as Problem
import qualified Solutions.P33         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Integer -> Bool) -> String -> Spec
properties coprime name = do
  describe name $ do
    prop "if greatest common divisor is 1" $
      \(Positive n) -> \(Positive m) -> coprime n m `shouldBe` gcd n m == 1

examples :: Spec
examples = do
  describe "Examples" $ do
    it "coprime 35 64" $ do
      coprime 35 (64 :: Integer) `shouldBe` True

  where coprime a b = Problem.coprime a b

spec :: Spec
spec = do
  properties Problem.coprime "coprime"
  examples
  describe "From solutions" $ do
    properties Solution.coprime "coprime"
