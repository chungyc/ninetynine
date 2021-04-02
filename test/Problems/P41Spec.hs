{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P41Spec (spec) where

import           Data.List             (sort)
import           Problems.P31
import qualified Problems.P41          as Problem
import qualified Solutions.P41         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Integer -> [(Integer, Integer)]) -> String -> Spec
properties goldbachList name = describe name $ do
  prop "are sums of primes" $
    \(Positive m) -> \(NonNegative n) ->
      goldbachList m (m+n) `shouldSatisfy` all (\(x,y) -> isPrime x && isPrime y)

  -- It might be possible there is an even number without a Goldbach pair,
  -- but it's really unlikely we'll encounter it with the test.
  prop "includes sums of all non-prime even natural numbers in range" $
    \(Positive m) -> \(NonNegative n) ->
      goldbachList m (m+n) `shouldSatisfy`
      (==) [k | k <- [m..m+n], k > 2 && even k] . sort . map (\(x,y) -> x+y)

examples :: Spec
examples = describe "Examples" $ do
  it "goldbachList 9 20" $ do
    goldbachList 9 20 `shouldSatisfy` all (\(x,y) -> isPrime x && isPrime y)
    goldbachList 9 20 `shouldSatisfy` (==) [10,12,14,16,18,20] . sort . map (\(x,y) -> x+y)

  it "filter (\\(m,n) -> m > 100 && n > 100) $ goldbachList 2 3000" $ do
    map (\(m,n) -> m+n) (filter (\(m,n) -> m > 100 && n > 100) (goldbachList 2 3000)) `shouldBe` [103+2539]

  where goldbachList m n = Problem.goldbachList m n :: [(Int,Int)]

spec :: Spec
spec = parallel $ do
  properties Problem.goldbachList "goldbachList"
  examples
  describe "From solutions" $ do
    properties Solution.goldbachList "goldbachlist"
