{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P43Spec (spec) where

import           Data.Complex
import           Data.Foldable         (for_)
import qualified Problems.P43          as Problem
import           Solutions.Arithmetic  (gaussianMultiply, gaussianUnits)
import qualified Solutions.P43         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Complex Integer -> Complex Integer -> Bool) -> String -> Spec
properties gaussianDividesBy name = describe name $ do
  prop "does not divide by zero" $ withMaxSuccess 10 $
    \x -> x `gaussianDividesBy` (0 :+ 0) `shouldBe` False

  context "with units" $ do
    for_ gaussianUnits $ \unit ->
      prop ("divided by " ++ show unit) $ withMaxSuccess 10 $
        \x -> x `gaussianDividesBy` unit `shouldBe` True

  prop "divides multiple of divisor by divisor" $
    \x y -> y /= (0 :+ 0) ==>
    (x `gaussianMultiply` y) `gaussianDividesBy` y `shouldBe` True

  prop "no z such that x=y*z exists when x is not divided by y" $
    withMaxSuccess 10000 $
    \x y z -> not (x `gaussianDividesBy` y) && y /= (0 :+ 0) ==>
    y `gaussianMultiply` z `shouldSatisfy` (/=) x

examples :: Spec
examples = describe "Examples" $ do
  it "(10 :+ 0) `gaussianDividesBy` (2 :+ 0)" $ do
    (10 :+ 0) `gaussianDividesBy` (2 :+ 0) `shouldBe` True

  it "(10 :+ 0) `gaussianDividesBy` (0 :+ 2)" $ do
    (10 :+ 0) `gaussianDividesBy` (0 :+ 2) `shouldBe` True

  it "(5 :+ 2) `gaussianDividesBy` (2 :+ (-1))" $ do
    (5 :+ 2) `gaussianDividesBy` (2 :+ (-1)) `shouldBe` False

  where gaussianDividesBy = Problem.gaussianDividesBy

spec :: Spec
spec = parallel $ do
  properties Problem.gaussianDividesBy "gaussianDividesBy"
  examples
  describe "From solutions" $ do
    properties Solution.gaussianDividesBy "gaussianDividesBy"
