{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P40Spec (spec) where

import           Problems.P31
import qualified Problems.P40          as Problem
import qualified Solutions.P40         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> (Integer, Integer)) -> String -> Spec
properties goldbach name = describe name $ do
  prop "sums up to given even number" $
    forAll ((*2) . (+2) <$> arbitrarySizedNatural) $ \n ->
    goldbach n `shouldSatisfy` \(x,y) -> x+y == n

  prop "are prime numbers" $
    forAll ((*2) . (+2) <$> arbitrarySizedNatural) $ \n ->
    goldbach n `shouldSatisfy` \(x,y) -> isPrime x && isPrime y

examples :: Spec
examples = describe "Examples" $ do
  it "goldbach 12" $ do
    goldbach 12 `shouldBe` (5,7)

  where goldbach n = Problem.goldbach (n :: Integer)

spec :: Spec
spec = parallel $ do
  properties Problem.goldbach "goldbach"
  examples
  describe "From solutions" $ do
    properties Solution.goldbach "goldbach"

