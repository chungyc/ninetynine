{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P35Spec (spec) where

import           Data.List             (sort)
import           Problems.P31
import qualified Problems.P35          as Problem
import qualified Solutions.P35         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> [Integer]) -> String -> Spec
properties primeFactors name = describe name $ do
  prop "has product equal to original number" $
    \(Positive n) -> primeFactors n `shouldSatisfy` (==) n . product

  prop "has prime factors" $
    \(Positive n) -> primeFactors n `shouldSatisfy` all isPrime

  prop "is in ascending order" $
    \(Positive n) -> primeFactors n `shouldBe` sort (primeFactors n)

examples :: Spec
examples = describe "Examples" $ do
  it "primeFactors 315" $ do
    primeFactors 315 `shouldBe` [3, 3, 5, 7]

  where primeFactors n = Problem.primeFactors n :: [Int]

spec :: Spec
spec = parallel $ do
  properties Problem.primeFactors "primeFactors"
  examples
  describe "From solutions" $ do
    properties Solution.primeFactors "primeFactors"
