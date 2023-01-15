{-|
Copyright: Copyright (C) 2023 Yoo Chung
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
properties primeFactorsMultiplicity name = describe name $ do
  prop "has product equal to original number" $ \(Positive n) ->
    primeFactorsMultiplicity n `shouldSatisfy` (==) n . product . map (\(p, r) -> p^r)

  prop "are prime factors" $ \(Positive n) ->
    primeFactorsMultiplicity n `shouldSatisfy` all (isPrime . fst)

  prop "are distinct factors" $ \(Positive n) ->
    primeFactorsMultiplicity n `shouldSatisfy` all ((==) 1 . length) . group . map fst

examples :: Spec
examples = describe "Examples" $ do
  it "primeFactorsMultiplicity 315" $ do
    primeFactorsMultiplicity 315 `shouldBe` [(3,2),(5,1),(7,1)]

  where primeFactorsMultiplicity n = Problem.primeFactorsMultiplicity (n :: Int)

spec :: Spec
spec = parallel $ do
  properties Problem.primeFactorsMultiplicity "primeFactorsMultiplicity"
  examples
  describe "From solutions" $ do
    properties Solution.primeFactorsMultiplicity "primeFactorsMultiplicity"
