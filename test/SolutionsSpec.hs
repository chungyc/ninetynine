{- |
Description : Ninety-Nine Haskell Solutions
Maintainer  : dev@chungyc.org

The [99 Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems) expressed in testable form.
-}
module SolutionsSpec (spec) where

import           Solutions
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Problem 1" $ do
    it "returns last element in non-empty list" $ property $ do
      \xs -> \x -> myLast (xs ++ [x]) === (x :: Int)

    it "has example: myLast [1,2,3,4]" $ do
      myLast [1,2,3,4] `shouldBe` (4 :: Int)

    it "has example: myLast ['x','y','z']" $ do
      myLast ['x','y','z'] `shouldBe` 'z'

  describe "Problem 2" $ do
    it "returns the last but one element in non-empty list" $ property $ do
      \xs -> \x -> \y -> myButLast (xs ++ [x,y]) == (x :: Int)

    it "has example: myButLast [1,2,3,4]" $ do
      myButLast [1,2,3,4] `shouldBe` (3 :: Int)

    it "has example: myButLast ['a'..'z']" $ do
      myButLast ['a'..'z'] `shouldBe` 'y'

  describe "Problem 3" $ do
    it "returns the K'th element of a list" $ property $ do
      \(Positive k) -> \j ->
        let l = (replicate (k-1) 'x' ++ ['y'] ++ replicate j 'x')
        in elementAt l k `shouldBe` 'y'

    it "has example: elementAt [1,2,3] 2" $ do
      elementAt [1,2,3] 2 `shouldBe` (2 :: Int)

    it "has example: elementAt \"haskell\" 5" $ do
      elementAt "haskell" 5 `shouldBe` 'e'

  describe "Problem 4" $ do
    it "returns zero length for empty list" $ do
      myLength [] `shouldBe` 0

    it "satisfies induction" $ property $ do
      \l -> myLength ('x':l) `shouldBe` 1 + myLength l

    it "has example: myLength [123, 456, 789]" $ do
      myLength ([123, 456, 789] :: [Int]) `shouldBe` 3

    it "has example: myLength \"Hello, world!\"" $ do
      myLength "Hello, world!" `shouldBe` 13

  describe "Problem 5" $ do
    it "returns reversed list" $ property $ do
      \l -> let naiveReverse []     = []
                naiveReverse (x:xs) = (naiveReverse xs) ++ [x]
            in myReverse l `shouldBe` naiveReverse (l :: [Int])

    it "returns original from reversed list" $ property $ do
      \l -> myReverse (myReverse l) `shouldBe` (l :: [Int])

    it "has example: myReverse \"A man, a plan, a canal, panama!\"" $ do
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"

    it "has example: myReverse [1,2,3,4]" $ do
      myReverse [1,2,3,4] `shouldBe` ([4,3,2,1] :: [Int])

  describe "Problem 6" $ do
    it "returns true for even-length palindromes" $ property $ do
      \xs -> let palindrome = (xs :: [Int]) ++ myReverse xs
             in isPalindrome palindrome `shouldBe` True

    it "returns true for odd-length palindromes" $ property $ do
      \xs -> \x -> let palindrome = xs ++ [x :: Int] ++ myReverse xs
                   in isPalindrome palindrome `shouldBe` True

    it "returns false for non-palindromes" $ property $ do
      \xs -> (xs :: [Int]) /= myReverse xs ==> isPalindrome xs `shouldBe` False

    it "has example: isPalindrome [1,2,3]" $ do
      isPalindrome ([1,2,3] :: [Int]) `shouldBe` False

    it "has example: isPalindrome \"madamimadam\"" $ do
      isPalindrome "madamimadam" `shouldBe` True

    it "has example: isPalindrome [1,2,4,8,16,8,4,2,1]" $ do
      isPalindrome ([1,2,4,8,16,8,4,2,1] :: [Int]) `shouldBe` True
