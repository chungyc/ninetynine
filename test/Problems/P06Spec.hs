module Problems.P06Spec (spec) where

import           Problems.P05
import           Problems.P06
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "isPalindrome" $ do
    prop "returns true for even-length palindromes" $
      \xs -> let p = (xs :: [Int]) ++ myReverse xs
             in p `shouldSatisfy` isPalindrome

    prop "returns true for odd-length palindromes" $
      \xs -> \x -> let p = xs ++ [x :: Int] ++ myReverse xs
                   in p `shouldSatisfy` isPalindrome

    prop "returns false for non-palindromes" $
      \xs ->
        (xs :: [Int]) /= myReverse xs ==>
        xs `shouldNotSatisfy` isPalindrome

  describe "Examples" $ do
    it "isPalindrome [1,2,3]" $ do
      isPalindrome ([1,2,3] :: [Int]) `shouldBe` False

    it "isPalindrome \"madamimadam\"" $ do
      isPalindrome "madamimadam" `shouldBe` True

    it "isPalindrome [1,2,4,8,16,8,4,2,1]" $ do
      isPalindrome [1,2,4,8,16,8,4,2,1 :: Int] `shouldBe` True
