module Problems.P04Spec (spec) where

import qualified Problems.P04          as Problem
import qualified Solutions.P04         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> Int) -> String -> Spec
properties myLength name = do
  describe name $ do
    it "returns zero for empty list" $ do
      myLength [] `shouldBe` 0

    prop "satisfies induction step" $
      \l -> myLength (1:l) `shouldBe` 1 + myLength l

examples :: SpecWith ()
examples = let myLength = Problem.myLength in
  describe "Examples" $ do
    it "myLength [123, 456, 789]" $ do
      myLength ([123, 456, 789] :: [Int]) `shouldBe` 3

    it "myLength \"Hello, world!\"" $ do
      myLength "Hello, world!" `shouldBe` 13

spec :: Spec
spec = do
  properties Problem.myLength "myLength"
  examples

  describe "From solutions" $ do
    properties Solution.myLength "myLength"
    properties Solution.myLength "myLength'"
    properties Solution.myLength "myLength''"
