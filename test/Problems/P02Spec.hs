{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P02Spec (spec) where

import qualified Problems.P02          as Problem
import qualified Solutions.P02         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> Maybe Int) -> String -> Spec
properties myButLast name = describe name $ do
  prop "finds the last but one element in list" $ \xs x y ->
    myButLast (xs ++ [x,y]) `shouldBe` Just x

  prop "returns nothing for singleton list" $ \x ->
    myButLast [x] `shouldBe` Nothing

  prop "returns nothing for empty list" $
    myButLast [] `shouldBe` Nothing

examples :: Spec
examples = describe "Examples" $ do
  it "myButLast [1,2,3,4]" $ do
    myButLast [1,2,3,4] `shouldBe` Just (3 :: Int)

  it "myButLast ['a'..'z']" $ do
    myButLast ['a'..'z'] `shouldBe` Just 'y'

 where myButLast = Problem.myButLast

spec :: Spec
spec = parallel $ do
  properties Problem.myButLast "myButLast"
  examples
  describe "From solutions" $ do
    properties Solution.myButLast "myButLast"
