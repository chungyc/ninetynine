{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Description : Ninety-Nine Haskell Solutions
Maintainer  : dev@chungyc.org

The [99 Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems) expressed in testable form.
-}
module SolutionsSpec (spec) where

import           Generic.Random
import           Solutions
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (NestedList a) where
  arbitrary = genericArbitraryRec (9 % 8 % ()) `withBaseCase` (Elem <$> arbitrary)

spec :: Spec
spec = do
  describe "Problem 1" $ do
    describe "myLast" $ do
      prop "returns last element in list" $
        \xs -> \x -> myLast (xs ++ [x]) === (x :: Int)

    describe "Examples" $ do
      it "myLast [1,2,3,4]" $ do
        myLast [1,2,3,4] `shouldBe` (4 :: Int)

      it "myLast ['x','y','z']" $ do
        myLast ['x','y','z'] `shouldBe` 'z'

  describe "Problem 2" $ do
    describe "myButLast" $ do
      prop "returns the last but one element in list" $
        \xs -> \x -> \y -> myButLast (xs ++ [x,y]) == (x :: Int)

    describe "Examples" $ do
      it "myButLast [1,2,3,4]" $ do
        myButLast [1,2,3,4] `shouldBe` (3 :: Int)

      it "myButLast ['a'..'z']" $ do
        myButLast ['a'..'z'] `shouldBe` 'y'

  describe "Problem 3" $ do
    describe "elementAt" $ do
      prop "returns the K'th element of a list" $
        \(Positive k) -> \j ->
          let l = (replicate (k-1) 'x' ++ ['y'] ++ replicate j 'x')
          in elementAt l k `shouldBe` 'y'

    describe "Examples" $ do
      it "elementAt [1,2,3] 2" $ do
        elementAt [1,2,3] 2 `shouldBe` (2 :: Int)

      it "elementAt \"haskell\" 5" $ do
        elementAt "haskell" 5 `shouldBe` 'e'

  describe "Problem 4" $ do
    describe "myLength" $ do
      it "returns zero length for empty list" $ do
        myLength [] `shouldBe` 0

      prop "satisfies induction" $
        \l -> myLength ('x':l) `shouldBe` 1 + myLength l

    describe "Examples" $ do
      it "myLength [123, 456, 789]" $ do
        myLength ([123, 456, 789] :: [Int]) `shouldBe` 3

      it "myLength \"Hello, world!\"" $ do
        myLength "Hello, world!" `shouldBe` 13

  describe "Problem 5" $ do
    describe "myReverse" $ do
      prop "returns reversed list" $
        \l -> let naiveReverse []     = []
                  naiveReverse (x:xs) = (naiveReverse xs) ++ [x]
              in myReverse l `shouldBe` naiveReverse (l :: [Int])

      prop "returns original from reversed list" $
        \l -> myReverse (myReverse l) `shouldBe` (l :: [Int])

    describe "Examples" $ do
      it "myReverse \"A man, a plan, a canal, panama!\"" $ do
        myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"

      it "myReverse [1,2,3,4]" $ do
        myReverse [1,2,3,4] `shouldBe` ([4,3,2,1] :: [Int])

  describe "Problem 6" $ do
    describe "isPalindrome" $ do
      prop "returns true for even-length palindromes" $
        \xs -> let palindrome = (xs :: [Int]) ++ myReverse xs
               in isPalindrome palindrome `shouldBe` True

      prop "returns true for odd-length palindromes" $
        \xs -> \x -> let palindrome = xs ++ [x :: Int] ++ myReverse xs
                     in isPalindrome palindrome `shouldBe` True

      prop "returns false for non-palindromes" $
        \xs -> (xs :: [Int]) /= myReverse xs ==> isPalindrome xs `shouldBe` False

    describe "Examples" $ do
      it "isPalindrome [1,2,3]" $ do
        isPalindrome ([1,2,3] :: [Int]) `shouldBe` False

      it "isPalindrome \"madamimadam\"" $ do
        isPalindrome "madamimadam" `shouldBe` True

      it "isPalindrome [1,2,4,8,16,8,4,2,1]" $ do
        isPalindrome ([1,2,4,8,16,8,4,2,1] :: [Int]) `shouldBe` True

  describe "Problem 7" $ do
    describe "flatten" $ do
      prop "returns flattened list" $
        \xs -> let naiveFlatten (Elem x) = [x]
                   naiveFlatten (List l) = concat $ map naiveFlatten l
               in flatten xs `shouldBe` naiveFlatten (xs :: NestedList Int)

    describe "Examples" $ do
      it "flatten (Elem 5)" $ do
        flatten (Elem 5) `shouldBe` [5 :: Int]

      it "flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])" $ do
        flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
          `shouldBe` ([1,2,3,4,5] :: [Int])

      it "flatten (List [])" $ do
        flatten (List []) `shouldBe` ([] :: [Int])

  describe "Problem 8" $ do
    describe "compress" $ do
      prop "leaves no consecutive duplicates" $
        \l -> let consecutive []           = False
                  consecutive [_]          = False
                  consecutive (x:xs@(y:_)) = x == y || consecutive xs
              in classify (not . consecutive $ l) "trivial" $
                 compress (l :: [Int]) `shouldSatisfy` not . consecutive

      prop "leaves elements in same order" $
        \l -> let consume _ [] = []
                  consume x (y:ys)
                    | x == y    = consume x ys
                    | otherwise = (y:ys)
                  sameOrder ([], []) = True
                  sameOrder ([], _)  = False
                  sameOrder (_, [])  = False
                  sameOrder (x:xs, y:ys)
                    | x == y    = sameOrder (consume x xs, consume y ys)
                    | otherwise = False
              in (l, compress (l :: [Int])) `shouldSatisfy` sameOrder

    describe "Examples" $ do
      it "compress \"aaaabccaadeeee\"" $ do
        compress "aaaabccaadeeee" `shouldBe` "abcade"
