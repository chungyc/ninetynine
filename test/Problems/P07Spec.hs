module Problems.P07Spec (spec) where

import           Problems.P04
import           Problems.P07
import           Problems.P07Arbitrary   ()
import           Problems.P07Definitions
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "flatten" $ do
    prop "flattens a nested list" $
      \xs -> let naiveFlatten (Elem x) = [x :: Int]
                 naiveFlatten (List l) = concat $ map naiveFlatten l
             in flatten xs `shouldBe` naiveFlatten xs

    prop "keeps same number of elements" $
      \xs -> let count (Elem _) = 1
                 count (List l) = sum (map count l)
             in myLength (flatten xs) `shouldBe` count (xs :: NestedList Int)

  describe "Examples" $ do
    it "flatten (Elem 5)" $ do
      flatten (Elem 5) `shouldBe` [5 :: Int]

    it "flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])" $ do
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
        `shouldBe` [1,2,3,4,5 :: Int]

    it "flatten (List [])" $ do
      flatten (List []) `shouldBe` ([] :: [Int])
