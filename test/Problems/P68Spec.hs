{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P68Spec (spec) where

import           Problems.BinaryTrees
import           Problems.P54
import           Problems.P57
import qualified Problems.P68          as Problem
import qualified Solutions.P68         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Tree Int -> [Int], Tree Int -> [Int], [Int] -> [Int] -> Tree Int)
           -> (String, String, String)
           -> Spec
properties (inorder, preorder, ordersToTree) (nameInorder, namePreorder, nameOrdersToTree) = do
  describe nameInorder $ do
    prop "is in-order sequence" $
      \(DistinctTree t) -> inorder t `shouldBe` inorderSequence t

  describe namePreorder $ do
    prop "is pre-order sequence" $
      \(DistinctTree t) -> preorder t `shouldBe` preorderSequence t

  describe nameOrdersToTree $ do
    prop "reconstructs binary tree from sequences" $
      \(DistinctTree t) -> ordersToTree (inorder t) (preorder t) `shouldBe` t

examples :: Spec
examples = describe "Examples" $ do
  it "inorder tree1" $ do
    inorder tree1 `shouldBe` "dbeacgf"

  it "preorder tree1" $ do
    preorder tree1 `shouldBe` "abdecfg"
    preorder tree1 `shouldBe` "abdecfg"

  it "ordersToTree \"dbeacgf\" \"abdecfg\"" $ do
    ordersToTree "dbeacgf" "abdecfg" `shouldBe` tree1

  where inorder t = Problem.inorder t
        preorder t = Problem.preorder t
        ordersToTree xs ys = Problem.ordersToTree xs ys

spec :: Spec
spec = parallel $ do
  properties (Problem.inorder, Problem.preorder, Problem.ordersToTree) ("inorder", "preorder", "ordersToTree")
  examples
  describe "From solutions" $ do
    properties (Solution.inorder, Solution.preorder, Solution.ordersToTree) ("inorder", "preorder", "ordersToTree")

-- | Straightforward implementation is also definition.
inorderSequence :: Tree a -> [a]
inorderSequence Empty          = []
inorderSequence (Branch x l r) = inorderSequence l ++ [x] ++ inorderSequence r

-- | Straightforward implementation is also definition.
preorderSequence :: Tree a -> [a]
preorderSequence Empty = []
preorderSequence (Branch x l r) = [x] ++ preorderSequence l ++ preorderSequence r

-- | Ensures that all node values are distinct.
newtype DistinctTree = DistinctTree (Tree Int)
  deriving (Eq, Show)

instance Arbitrary DistinctTree where
  arbitrary = sized $ \n -> do
    xs <- shuffle [1..n]
    return $ DistinctTree $ construct xs
