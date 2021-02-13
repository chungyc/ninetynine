module Problems.P03Spec (spec) where

import           Problems.P03
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "elementAt" $ do
    prop "finds the K'th element of a list" $
      \(Positive k) -> \j -> \x -> \y ->
        x /= y ==>
        let l = (replicate (k-1) y ++ [x :: Int] ++ replicate j y)
        in elementAt l k `shouldBe` x

  describe "Examples" $ do
    it "elementAt [1,2,3] 2" $ do
      elementAt [1,2,3] 2 `shouldBe` (2 :: Int)

    it "elementAt \"haskell\" 5" $ do
      elementAt "haskell" 5 `shouldBe` 'e'
