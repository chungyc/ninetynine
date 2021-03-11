module Problems.P61Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import qualified Problems.P61                   as Problem
import qualified Solutions.P61                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

leavesProp :: (Tree Int -> [Int]) -> String -> Spec
leavesProp leaves name = do
  describe name $ do
    prop "are leaves" $
      \t -> leaves t `shouldSatisfy` all (isLeaf t)

    prop "has correct number of leaves" $
      \t -> length (leaves t) `shouldBe` countLeaves t

internalsProp :: (Tree Int -> [Int]) -> String -> Spec
internalsProp internals name = do
  describe name $ do
    prop "are internal nodes" $
      \t -> internals t `shouldSatisfy` all (isInternal t)

    prop "has correct number of internal nodes" $
      \t -> length (internals t) `shouldBe` (treeSize t - countLeaves t)

spec :: Spec
spec = parallel $ do
  leavesProp Problem.leaves "leaves"
  internalsProp Problem.internals "internals"
  describe "From solutions" $ do
    leavesProp Solution.leaves "leaves"

isLeaf :: Tree Int -> Int -> Bool
isLeaf Empty _                  = False
isLeaf (Branch x Empty Empty) n = x == n
isLeaf (Branch _ l r) n         = isLeaf l n || isLeaf r n

isInternal :: Tree Int -> Int -> Bool
isInternal Empty _                  = False
isInternal (Branch _ Empty Empty) _ = False
isInternal (Branch x l r) n         = x == n || isInternal l n || isInternal r n

countLeaves :: Tree Int -> Int
countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r)         = countLeaves l + countLeaves r
