{-|
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P44Spec (spec) where

import           Data.Complex
import qualified Problems.P44          as Problem
import qualified Solutions.P44         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Complex Integer -> Bool) -> String -> Spec
properties isGaussianPrime name = describe name $ do
  prop "is false for composite Gaussian integers" $
    \x y -> not (x == zero || y == zero || x `elem` units || y `elem` units) ==>
    isGaussianPrime (x `multiply` y) `shouldBe` False

  where zero = 0 :+ 0
        units = [1 :+ 0, (-1) :+ 0, 0 :+ 1, 0 :+ (-1)]
        multiply (a :+ b) (c :+ d) = (a*c-b*d) :+ (a*d+b*c)

examples :: Spec
examples = describe "Examples" $ do
  it "is pending" $ do
    pending

spec :: Spec
spec = parallel $ do
  properties Problem.isGaussianPrime "isGaussianPrime"
  examples
  describe "From solutions" $ do
    properties Solution.isGaussianPrime "isGaussianPrime"
