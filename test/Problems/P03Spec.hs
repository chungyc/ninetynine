{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P03Spec (spec) where

import qualified Problems.P03          as Problem
import qualified Solutions.P03         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Int -> Int) -> String -> Spec
properties elementAt name = describe name $ do
  prop "finds the K'th element of a list" $
    \(Positive k) -> \x ->
      forAll (vectorOf (k-1) arbitrary) $ \ys ->
      forAll (listOf arbitrary) $ \zs ->
      elementAt (ys ++ [x] ++ zs) k `shouldBe` x

examples :: Spec
examples = describe "Examples" $ do
  it "elementAt [1,2,3] 2" $ do
    elementAt [1,2,3] 2 `shouldBe` (2 :: Int)

  it "elementAt \"haskell\" 5" $ do
    elementAt "haskell" 5 `shouldBe` 'e'

  where elementAt = Problem.elementAt

spec :: Spec
spec = parallel $ do
  properties Problem.elementAt "elementAt"
  examples
  describe "From solutions" $ do
    properties Solution.elementAt "elementAt"
