{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P01Spec (spec) where

import qualified Problems.P01          as Problem
import qualified Solutions.P01         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> Maybe Int) -> String -> Spec
properties myLast name = describe name $ do
  prop "returns last element in list" $ \xs x ->
    myLast (xs ++ [x]) `shouldBe` Just x

  prop "returns nothing from empty list" $
    myLast [] `shouldBe` Nothing

examples :: Spec
examples = describe "Examples" $ do
  it "myLast [1,2,3,4]" $ do
    myLast [1,2,3,4] `shouldBe` Just (4 :: Int)

  it "myLast ['x','y','z']" $ do
    myLast ['x','y','z'] `shouldBe` Just 'z'

  it "myLast []" $ do
    myLast [] `shouldBe` (Nothing :: Maybe Bool)

  where myLast = Problem.myLast

spec :: Spec
spec = parallel $ do
  properties Problem.myLast "myLast"
  examples
  describe "From solutions" $ do
    properties Solution.myLast "myLast"
