module Problems.P01Spec (spec) where

import qualified Problems.P01          as Problem
import qualified Solutions.P01         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> Int) -> String -> Spec
properties myLast name = do
  describe name $ do
    prop "returns last element in list" $
      \xs -> \x ->
        let l = xs ++ [x :: Int]
        in myLast l `shouldBe` x

examples :: Spec
examples =
  describe "Examples" $ do
    it "myLast [1,2,3,4]" $ do
      myLast [1,2,3,4] `shouldBe` (4 :: Int)

    it "myLast ['x','y','z']" $ do
      myLast ['x','y','z'] `shouldBe` 'z'

  where myLast = Problem.myLast

spec :: Spec
spec = do
  properties Problem.myLast "myLast"
  examples
  describe "From solutions" $ do
    properties Solution.myLast "myLast"
