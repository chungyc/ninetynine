{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P36Spec (spec) where

import           Data.List             (group)
import           Problems.P31
import qualified Problems.P36          as Problem
import qualified Solutions.P36         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> [(Integer, Integer)]) -> String -> Spec
properties primeFactorsMultiplicity name = do
  describe name $ do
    prop "are factors of original number" $
      \(Positive n) -> product (map (\(p, r) -> p^r) $ primeFactorsMultiplicity n) `shouldBe` n

    prop "are prime factors" $
      \(Positive n) -> map fst (primeFactorsMultiplicity n) `shouldSatisfy` all isPrime

    prop "are distinct factors" $
      \(Positive n) -> map fst (primeFactorsMultiplicity n) `shouldSatisfy` all ((==) 1 . length) . group

examples :: Spec
examples = do
  describe "Examples" $ do
    it "primeFactorsMultiplicity 315" $ do
      primeFactorsMultiplicity (315 :: Integer) `shouldBe` [(3,2),(5,1),(7,1)]

  where primeFactorsMultiplicity n = Problem.primeFactorsMultiplicity n

spec :: Spec
spec = parallel $ do
  properties Problem.primeFactorsMultiplicity "primeFactorsMultiplicity"
  examples
  describe "From solutions" $ do
    properties Solution.primeFactorsMultiplicity "primeFactorsMultiplicity"
