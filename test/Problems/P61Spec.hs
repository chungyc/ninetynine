module Problems.P61Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import qualified Problems.P61                   as Problem
import qualified Solutions.P61                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: (Tree Int -> [Int]) -> String -> Spec
properties leaves name = do
  describe name $ do
    prop "are leaves" $
      \t -> leaves t `shouldSatisfy` all (isLeaf t)

    prop "has correct number of leaves" $
      \t -> length (leaves t) `shouldBe` countLeaves t

spec :: Spec
spec = parallel $ do
  properties Problem.leaves "leaves"
  describe "From solutions" $ do
    properties Solution.leaves "leaves"

isLeaf :: Tree Int -> Int -> Bool
isLeaf Empty _                  = False
isLeaf (Branch x Empty Empty) n = x == n
isLeaf (Branch _ l r) n         = isLeaf l n || isLeaf r n

countLeaves :: Tree Int -> Int
countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r)         = countLeaves l + countLeaves r
