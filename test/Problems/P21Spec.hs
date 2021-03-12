module Problems.P21Spec (spec) where

import qualified Problems.P21          as Problem
import qualified Solutions.P21         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: (Int -> [Int] -> Int -> [Int]) -> String -> Spec
properties insertAt name = do
  describe name $ do
    prop "inserts at given position" $
      \xs -> \ys -> \z ->
        let vs = xs ++ ys
            n = length xs + 1
        in insertAt z vs n `shouldBe` xs ++ [z] ++ ys

examples :: Spec
examples = do
  describe "Examples" $ do
    it "insertAt 'X' \"abcd\" 2" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

  where insertAt = Problem.insertAt

spec :: Spec
spec = do
  properties Problem.insertAt "insertAt"
  examples
  describe "From solutions" $ do
    properties Solution.insertAt "insertAt"
