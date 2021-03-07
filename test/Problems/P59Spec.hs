module Problems.P59Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import qualified Problems.P59                   as Problem
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> [Tree ()]) -> String -> Spec
properties heightBalancedTrees name = do
  describe name $ do
    modifyMaxSize (const 6) $ modifyMaxSuccess (const 15) $ do
      prop "has trees with expected height" $
        \(Positive n) -> heightBalancedTrees n `shouldSatisfy` all ((==) n . height)

      prop "has trees which are height balanced" $
        \(Positive n) -> heightBalancedTrees n `shouldSatisfy` all isHeightBalanced

    modifyMaxSize (const 30) $ do
      prop "includes tree if and only if it is height balanced" $
        \t -> height t > 1 && height t <= 4 ==>  -- not trivial, and not too slow
              classify (isHeightBalanced t) "balanced" $
              elem t (heightBalancedTrees $ height t) `shouldBe` isHeightBalanced t

spec :: Spec
spec = parallel $ do
  properties Problem.heightBalancedTrees "heightBalancedTrees"

height :: Tree a -> Int
height Empty          = 0
height (Branch _ l r) = 1 + max (height l) (height r)

isHeightBalanced :: Tree a -> Bool
isHeightBalanced Empty = True
isHeightBalanced (Branch _ l r) =
  abs (height l - height r) <= 1 &&
  isHeightBalanced l &&
  isHeightBalanced r
