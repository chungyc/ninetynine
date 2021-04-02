{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P02Spec (spec) where

import qualified Problems.P02          as Problem
import qualified Solutions.P02         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> Int) -> String -> Spec
properties myButLast name = do
  describe name $ do
    prop "finds the last but one element in list" $
      \xs -> \x -> \y ->
        myButLast (xs ++ [x,y :: Int]) `shouldBe` x

examples :: Spec
examples =
  describe "Examples" $ do
    it "myButLast [1,2,3,4]" $ do
      myButLast [1,2,3,4] `shouldBe` (3 :: Int)

    it "myButLast ['a'..'z']" $ do
      myButLast ['a'..'z'] `shouldBe` 'y'

 where myButLast = Problem.myButLast

spec :: Spec
spec = parallel $ do
  properties Problem.myButLast "myButLast"
  examples
  describe "From solutions" $ do
    properties Solution.myButLast "myButLast"
