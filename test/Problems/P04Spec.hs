module Problems.P04Spec (spec) where

import           Problems.P04
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "myLength" $ do
    it "returns zero for empty list" $ do
      myLength [] `shouldBe` 0

    prop "satisfies induction step" $
      \l -> myLength ('x':l) `shouldBe` 1 + myLength l

  describe "Examples" $ do
    it "myLength [123, 456, 789]" $ do
      myLength ([123, 456, 789] :: [Int]) `shouldBe` 3

    it "myLength \"Hello, world!\"" $ do
      myLength "Hello, world!" `shouldBe` 13
