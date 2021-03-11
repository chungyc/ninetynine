module Problems.P62Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import           Problems.P54
import qualified Problems.P62                   as Problem
import qualified Solutions.P62                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Tree Int -> Int -> [Int]) -> String -> Spec
properties atLevel name = do
  describe name $ do
    prop "includes all nodes at specific level" $
      \t -> forAll (chooseInt (1, treeHeight t)) $ \n ->
        t /= Empty ==>
        atLevel t n `shouldMatchList` map last (filter (\p -> n == levelFromPath p) $ pathsFromRoot t)

examples :: Spec
examples = do
  describe "Examples" $ do
    it "atLevel tree4 2" $ do
      atLevel tree4 2 `shouldBe` [2,2]

  where atLevel = Problem.atLevel

spec :: Spec
spec = do
  properties Problem.atLevel "atLevel"
  examples
  describe "From solutions" $ do
    properties Solution.atLevel "atLevel"

-- By definition.
levelFromPath :: [a] -> Int
levelFromPath path = length path

pathsFromRoot :: Tree a -> [[a]]
pathsFromRoot Empty = [[]]
pathsFromRoot (Branch x Empty Empty) = [[x]]
pathsFromRoot (Branch x l Empty) = (:) [x] $ map (x:) $ pathsFromRoot l
pathsFromRoot (Branch x Empty r) = (:) [x] $ map (x:) $ pathsFromRoot r
pathsFromRoot (Branch x l r) = (:) [x] $ concat $ map (map (x:) . pathsFromRoot) [l, r]
