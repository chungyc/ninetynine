{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P06Spec (spec) where

import qualified Problems.P06          as Problem
import qualified Solutions.P06         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Bool) -> String -> Spec
properties isPalindrome name = describe name $ do
  prop "returns true for even-length palindromes" $
    \xs -> isPalindrome (xs ++ reverse xs) `shouldBe` True

  prop "returns true for odd-length palindromes" $
    \xs -> \x -> isPalindrome (xs ++ [x] ++ reverse xs) `shouldBe` True

  prop "returns false for non-palindromes" $
    \xs -> xs /= reverse xs ==> isPalindrome xs `shouldBe` False

examples :: Spec
examples = describe "Examples" $ do
  it "isPalindrome [1,2,3]" $ do
    isPalindrome [1,2,3 :: Int] `shouldBe` False

  it "isPalindrome \"madamimadam\"" $ do
    isPalindrome "madamimadam" `shouldBe` True

  it "isPalindrome [1,2,4,8,16,8,4,2,1]" $ do
    isPalindrome [1,2,4,8,16,8,4,2,1 :: Int] `shouldBe` True

  where isPalindrome l = Problem.isPalindrome l

spec :: Spec
spec = parallel $ do
  properties Problem.isPalindrome "isPalindrome"
  examples
  describe "From solutions" $ do
    properties Solution.isPalindrome "isPalindrome"
