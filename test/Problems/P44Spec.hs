{-|
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P44Spec (spec) where

import           Data.Complex
import           Data.Foldable         (for_)
import           Problems.P43          (gaussianDividesBy)
import qualified Problems.P44          as Problem
import qualified Solutions.P44         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Complex Integer -> Bool) -> String -> Spec
properties isGaussianPrime name = describe name $ do
  it "zero is not prime" $ do
    isGaussianPrime (0:+0) `shouldBe` False

  context "with units" $ do
    for_ units $ \x -> it (show x ++ " is not prime") $ isGaussianPrime x `shouldBe` False

  prop "is false for composite Gaussian integers" $
    \x y -> all (not . shouldBeExcluded) [x,y]  ==>
    counterexample (concat [show $ multiply x y, " = ", show x, " * ", show y]) $
    isGaussianPrime (x `multiply` y) `shouldBe` False

  prop "does not have non-unit proper divisor when prime" $ withMaxSuccess 10000 $
    \x y -> all (not . shouldBeExcluded) [x,y] ==>
            not (isAssociate x y) ==>
            isGaussianPrime x ==>
    x `gaussianDividesBy` y `shouldBe` False

  where zero = 0 :+ 0
        units = [1 :+ 0, (-1) :+ 0, 0 :+ 1, 0 :+ (-1)]
        multiply (a :+ b) (c :+ d) = (a*c-b*d) :+ (a*d+b*c)
        shouldBeExcluded x = x == zero || x `elem` units
        isAssociate x y = elem y $ map (multiply x) [ 1:+0, (-1):+0, 0:+1, 0:+(-1) ]

examples :: Spec
examples = describe "Examples" $ do
  it "isGaussianPrime (0 :+ 5)" $ do
    isGaussianPrime (0 :+ 5) `shouldBe` False

  it "isGaussianPrime (5 :+ 2)" $ do
    isGaussianPrime (5 :+ 2) `shouldBe` True

  it "isGaussianPrime ((-2) :+ 5)" $ do
    isGaussianPrime ((-2) :+ 5) `shouldBe` True

  it "isGaussianPrime (17 :+ 0)" $ do
    isGaussianPrime (17 :+ 0) `shouldBe` False

  where isGaussianPrime = Problem.isGaussianPrime

spec :: Spec
spec = parallel $ do
  properties Problem.isGaussianPrime "isGaussianPrime"
  examples
  describe "From solutions" $ do
    properties Solution.isGaussianPrime "isGaussianPrime"
