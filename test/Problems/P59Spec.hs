module Problems.P59Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import qualified Problems.P59                   as Problem
import qualified Solutions.P59                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> [Tree ()]) -> String -> Spec
properties heightBalancedTrees name = do
  describe name $ do
    modifyMaxSize (const 6) $ modifyMaxSuccess (const 15) $ do
      prop "has trees with expected height" $
        \(Positive n) -> heightBalancedTrees n `shouldSatisfy` all ((==) n . treeHeight)

      prop "has trees which are height balanced" $
        \(Positive n) -> heightBalancedTrees n `shouldSatisfy` all isHeightBalanced

    modifyMaxSize (const 30) $ do
      prop "includes tree if and only if it is height balanced" $
        \t -> treeHeight t > 1 && treeHeight t <= 4 ==>  -- not trivial, and not too slow
              classify (isHeightBalanced t) "balanced" $
              elem t (heightBalancedTrees $ treeHeight t) `shouldBe` isHeightBalanced t

examples :: Spec
examples = do
  describe "Examples" $ do
    it "heightBalancedTrees 2" $ do
      heightBalancedTrees 2 `shouldMatchList`
        [ Branch () (Branch () Empty Empty) Empty
        , Branch () (Branch () Empty Empty) (Branch () Empty Empty)
        , Branch () Empty (Branch () Empty Empty)]

    it "length $ heightBalancedTrees 4" $ do
      length (heightBalancedTrees 4) `shouldBe` 315

  where heightBalancedTrees = Problem.heightBalancedTrees

spec :: Spec
spec = parallel $ do
  properties Problem.heightBalancedTrees "heightBalancedTrees"
  examples
  describe "From solutions" $ do
    properties Solution.heightBalancedTrees  "heightBalancedTrees"

isHeightBalanced :: Tree a -> Bool
isHeightBalanced Empty = True
isHeightBalanced (Branch _ l r) =
  abs (treeHeight l - treeHeight r) <= 1 &&
  isHeightBalanced l &&
  isHeightBalanced r
