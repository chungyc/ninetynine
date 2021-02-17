module Problems.P31Spec (spec) where

import qualified Problems.P31          as Problem
import qualified Solutions.P31         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Bool) -> String -> Spec
properties isPrime name = do
  describe name $ do
    prop "if and only if there are exactly two divisors" $
      \(Positive n) -> isPrime n `shouldBe` [1, n] == filter (\k -> n `mod` k == 0) [1..n]

examples :: Spec
examples = do
  describe "Examples" $ do
    it "isPrime 7" $ do
      isPrime (7 :: Int) `shouldBe` True

    it "isPrime 8" $ do
      isPrime (15 :: Int) `shouldBe` False

  where isPrime n = Problem.isPrime n

spec :: Spec
spec = do
  properties Problem.isPrime "isPrime"
  examples
  describe "From solutions" $ do
    properties Solution.isPrime   "isPrime"
    properties Solution.isPrime'  "isPrime'"
    properties Solution.isPrime'' "isPrime''"
