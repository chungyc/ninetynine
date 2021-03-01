module Problems.P07Spec (spec) where

import           Problems.Lists
import           Problems.Lists.Arbitrary ()
import qualified Problems.P07             as Problem
import qualified Solutions.P07            as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: (NestedList Int -> [Int]) -> String -> Spec
properties flatten name = do
  describe name $ do
    prop "flattens a nested list" $
      let naiveFlatten (Elem x) = [x]
          naiveFlatten (List l) = concat $ map naiveFlatten l
      in \xs -> flatten xs `shouldBe` naiveFlatten xs

    prop "keeps same number of elements" $
      let count (Elem _) = 1
          count (List l) = sum (map count l)
      in \xs -> length (flatten xs) `shouldBe` count xs

examples :: Spec
examples =
  describe "Examples" $ do
    it "flatten (Elem 5)" $ do
      flatten (Elem 5) `shouldBe` [5 :: Int]

    it "flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])" $ do
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
        `shouldBe` [1,2,3,4,5 :: Int]

    it "flatten (List [])" $ do
      flatten (List []) `shouldBe` ([] :: [Int])

  where flatten = Problem.flatten

spec :: Spec
spec = do
  properties Problem.flatten "flatten"
  examples
  describe "From solutions" $ do
    properties Solution.flatten  "flatten"
    properties Solution.flatten' "flatten'"
