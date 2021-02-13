module Problems.P02Spec (spec) where

import           Problems.P02
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "myButLast" $ do
    prop "finds the last but one element in list" $
      \xs -> \x -> \y ->
        myButLast (xs ++ [x,y :: Int]) `shouldBe` x

  describe "Examples" $ do
    it "myButLast [1,2,3,4]" $ do
      myButLast [1,2,3,4] `shouldBe` (3 :: Int)

    it "myButLast ['a'..'z']" $ do
      myButLast ['a'..'z'] `shouldBe` 'y'
