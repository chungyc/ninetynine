{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P44Spec (spec) where

import           Data.Complex
import           Data.Foldable         (for_)
import           Problems.P43          (gaussianDividesBy)
import qualified Problems.P44          as Problem
import           Solutions.Arithmetic  (gaussianMultiply, gaussianUnits)
import qualified Solutions.P44         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Complex Integer -> Bool) -> String -> Spec
properties isGaussianPrime name = describe name $ do
  prop "zero is not prime" $
    isGaussianPrime (0:+0) `shouldBe` False

  context "with units" $ do
    for_ gaussianUnits $ \x ->
      prop (show x ++ " is not prime") $
        isGaussianPrime x `shouldBe` False

  prop "is false for composite Gaussian integers" $ \x y ->
    not (any shouldBeExcluded [x,y])  ==>
    counterexample (concat [show $ gaussianMultiply x y, " = ", show x, " * ", show y]) $
    isGaussianPrime (x `gaussianMultiply` y) `shouldBe` False

  prop "does not have non-unit proper divisor when prime" $ \x y ->
    not (any shouldBeExcluded [x,y]) ==>
    not (isAssociate x y) ==>
    isGaussianPrime x ==>
    x `gaussianDividesBy` y `shouldBe` False

  where shouldBeExcluded x = x == (0 :+ 0) || x `elem` gaussianUnits
        isAssociate x y = elem y $ map (gaussianMultiply x) gaussianUnits

examples :: Spec
examples = describe "Examples" $ do
  it "isGaussianPrime (0 :+ 5)" $ do
    isGaussianPrime (0 :+ 5) `shouldBe` False

  it "isGaussianPrime (17 :+ 0)" $ do
    isGaussianPrime (17 :+ 0) `shouldBe` False

  it "isGaussianPrime (5 :+ 2)" $ do
    isGaussianPrime (5 :+ 2) `shouldBe` True

  where isGaussianPrime = Problem.isGaussianPrime

spec :: Spec
spec = parallel $ do
  properties Problem.isGaussianPrime "isGaussianPrime"
  examples
  describe "From solutions" $ do
    properties Solution.isGaussianPrime "isGaussianPrime"
