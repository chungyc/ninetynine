{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P20Spec (spec) where

import qualified Problems.P20          as Problem
import qualified Solutions.P20         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: (Int -> [Int] -> (Int,[Int])) -> String -> Spec
properties removeAt name = describe name $ do
  prop "removes k'th element from list" $ \xs -> \y -> \zs ->
    removeAt (length xs + 1) (xs ++ [y] ++ zs) `shouldBe` (y, xs ++ zs)

examples :: Spec
examples = describe "Examples" $ do
  it "removeAt 2 \"abcd\"" $ do
    removeAt 2 "abcd" `shouldBe` ('b',"acd")

  where removeAt = Problem.removeAt

spec :: Spec
spec = parallel $ do
  properties Problem.removeAt "removeAt"
  examples
  describe "From solutions" $ do
    properties Solution.removeAt "removeAt"
