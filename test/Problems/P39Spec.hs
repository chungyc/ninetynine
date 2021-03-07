module Problems.P39Spec (spec) where

import           Problems.P31
import qualified Problems.P39          as Problem
import qualified Solutions.P39         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Integer -> [Integer], [Integer]) -> (String, String) -> Spec
properties (primesR, primes) (namePrimesR, namePrimes) = do
  describe namePrimesR $ do
    prop "includes prime numbers in range" $
      \(Positive x) -> \(Positive y) -> primesR x (x+y) `shouldBe` naivePrimesR x (x+y)

  describe namePrimes $ do
    it "includes prime numbers" $ do
      take 10000 primes `shouldBe` take 10000 naivePrimes

  where naivePrimesR lo hi = filter isPrime [lo..hi]
        naivePrimes = filter isPrime [1..]

examples :: Spec
examples = do
  describe "Examples" $ do
    it "primesR 10 20" $ do
      primesR 10 20 `shouldBe` [11,13,17,19 :: Int]

    it "take 5 primes" $ do
      take 5 primes `shouldBe` [2,3,5,7,11 :: Int]

  where primesR lo hi = Problem.primesR lo hi
        primes = Problem.primes

spec :: Spec
spec = parallel $ do
  properties (Problem.primesR, Problem.primes) ("primesR", "primes")
  examples
  describe "From solutions" $ do
    properties (Solution.primesR, Solution.primes) ("primesR", "primes")
