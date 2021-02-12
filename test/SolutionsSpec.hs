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
