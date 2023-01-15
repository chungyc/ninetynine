{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
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
    prop "includes prime numbers in range" $ \(Positive n) -> \(NonNegative k) ->
      primesR n (n+k) `shouldBe` filter isPrime [n..n+k]

  describe namePrimes $ do
    prop "is not bounded" $ \(Positive n) ->
      take n primes `shouldSatisfy` (==) n . length

    prop "is in sorted order" $ \(Positive n) -> \(Positive k) ->
      (primes !! n, primes !! n+k) `shouldSatisfy` \(x,y) -> x < y

    prop "includes prime numbers" $ \(Positive n) ->
      isPrime n ==>
      takeWhile (<=n) primes `shouldSatisfy` elem n

    prop "does not include numbers which are not prime" $ \(Positive n) ->
      not (isPrime n) ==>
      takeWhile (<=n) primes `shouldSatisfy` not . elem n

examples :: Spec
examples = describe "Examples" $ do
  it "primesR 10 20" $ do
    primesR 10 20 `shouldBe` [11,13,17,19]

  it "take 5 primes" $ do
    take 5 primes `shouldBe` [2,3,5,7,11]

  where primesR lo hi = Problem.primesR lo hi :: [Int]
        primes = Problem.primes :: [Int]

spec :: Spec
spec = parallel $ do
  properties (Problem.primesR, Problem.primes) ("primesR", "primes")
  examples
  describe "From solutions" $ do
    properties (Solution.primesR, Solution.primes) ("primesR", "primes")
