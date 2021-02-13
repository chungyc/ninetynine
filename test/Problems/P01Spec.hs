module Problems.P01Spec (spec) where

import           Problems.P01
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "myLast" $ do
    prop "returns last element in list" $
      \xs -> \x ->
        let l = xs ++ [x :: Int]
        in myLast l `shouldBe` x

  describe "Examples" $ do
    it "myLast [1,2,3,4]" $ do
      myLast [1,2,3,4] `shouldBe` (4 :: Int)

    it "myLast ['x','y','z']" $ do
      myLast ['x','y','z'] `shouldBe` 'z'
