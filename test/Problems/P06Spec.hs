module Problems.P06Spec (spec) where

import qualified Problems.P06          as Problem
import qualified Solutions.P06         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Bool) -> String -> Spec
properties isPalindrome name = do
  describe name $ do
    prop "returns true for even-length palindromes" $
      \xs -> let p = xs ++ reverse xs
             in p `shouldSatisfy` isPalindrome

    prop "returns true for odd-length palindromes" $
      \xs -> \x -> let p = xs ++ [x] ++ reverse xs
                   in p `shouldSatisfy` isPalindrome

    prop "returns false for non-palindromes" $
      \xs -> xs /= reverse xs ==>
             xs `shouldNotSatisfy` isPalindrome

examples :: Spec
examples =
  describe "Examples" $ do
    it "isPalindrome [1,2,3]" $ do
      isPalindrome [1,2,3 :: Int] `shouldBe` False

    it "isPalindrome \"madamimadam\"" $ do
      isPalindrome "madamimadam" `shouldBe` True

    it "isPalindrome [1,2,4,8,16,8,4,2,1]" $ do
      isPalindrome [1,2,4,8,16,8,4,2,1 :: Int] `shouldBe` True

  where isPalindrome l = Problem.isPalindrome l

spec :: Spec
spec = do
  properties Problem.isPalindrome "isPalindrome"
  examples
  describe "From solutions" $ do
    properties Solution.isPalindrome "isPalindrome"
