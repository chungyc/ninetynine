module Problems.P35Spec (spec) where

import           Data.List             (sort)
import           Problems.P31
import qualified Problems.P35          as Problem
import qualified Solutions.P35         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> [Integer]) -> String -> Spec
properties primeFactors name = do
  describe name $ do
    prop "has product equal to original number" $
      \(Positive n) -> product (primeFactors n) `shouldBe` n

    prop "has prime factors" $
      \(Positive n) -> conjoin (map (flip shouldSatisfy isPrime) (primeFactors n))

    prop "is in ascending order" $
      \(Positive n) -> primeFactors n `shouldBe` sort (primeFactors n)

examples :: Spec
examples = do
  describe "Examples" $ do
    it "primeFactors 315" $ do
      primeFactors 315 `shouldBe` [3, 3, 5, 7 :: Int]

  where primeFactors n = Problem.primeFactors n

spec :: Spec
spec = parallel $ do
  properties Problem.primeFactors "primeFactors"
  examples
  describe "From solutions" $ do
    properties Solution.primeFactors "primeFactors"
