{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P63Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import qualified Problems.P63                   as Problem
import qualified Solutions.P63                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> Tree (), Tree Int -> Bool) -> (String, String) -> Spec
properties
  (completeBinaryTree, isCompleteBinaryTree)
  (completeBinaryTreeName, isCompleteBinaryTreeName) = do
  describe completeBinaryTreeName $ do
    it "is empty with zero nodes" $ do
      completeBinaryTree 0 `shouldBe` Empty

    prop "has equal complete subtrees" $ \(Tiny k) ->
      k > 0 ==>
      completeBinaryTree (2^k-1) `shouldBe`
      Branch () (completeBinaryTree $ 2^(k-1)-1) (completeBinaryTree $ 2^(k-1)-1)

    prop "has incomplete left subtree and complete right subtree" $ \(Tiny k) ->
      k > 1 ==> forAll (choose (1, 2^(k-1))) $ \i ->
      completeBinaryTree (2^k - 1 + i) `shouldBe`
      Branch () (completeBinaryTree $ 2^(k-1) - 1 + i) (completeBinaryTree $ 2^(k-1) - 1)

    prop "has complete left tree and incomplete right subtree" $ \(Tiny k) ->
      k > 1 ==> forAll (choose (1, 2^(k-1))) $ \i ->
      completeBinaryTree (2^k - 1 + 2^(k-1) + i) `shouldBe`
      Branch () (completeBinaryTree $ 2^k - 1) (completeBinaryTree $ 2^(k-1) - 1 + i)

    prop "has given number of nodes" $
      \(NonNegative n) -> completeBinaryTree n `shouldSatisfy` (==) n . treeSize

  describe isCompleteBinaryTreeName $ do
    it "is true for empty tree" $ do
      isCompleteBinaryTree Empty `shouldBe` True

    prop "is true for singleton tree" $
      \x -> isCompleteBinaryTree (Branch x Empty Empty) `shouldBe` True

    prop "is true only for complete binary trees" $ \t ->
      treeHeight t > 1 ==>
      classify (isCompleteBinaryTree t) "complete binary tree" $
      isCompleteBinaryTree t `shouldBe`
      stripValue t == completeBinaryTree (treeSize t)

examples :: Spec
examples = describe "Examples" $ do
  it "completeBinaryTree 4" $ do
    completeBinaryTree 4 `shouldBe`
      Branch () (Branch () (Branch () Empty Empty) Empty) (Branch () Empty Empty)

  it "isCompleteBinaryTree $ Branch () (Branch () Empty Empty) Empty" $ do
    isCompleteBinaryTree (Branch () (Branch () Empty Empty) Empty) `shouldBe` True

  it "isCompleteBinaryTree $ Branch () Empty (Branch () Empty Empty)" $ do
    isCompleteBinaryTree (Branch () Empty (Branch () Empty Empty)) `shouldBe` False

  where completeBinaryTree = Problem.completeBinaryTree
        isCompleteBinaryTree = Problem.isCompleteBinaryTree

spec :: Spec
spec = parallel $ do
  properties
    (Problem.completeBinaryTree, Problem.isCompleteBinaryTree)
    ("completeBinaryTree", "isCompleteBinaryTree")
  examples
  describe "From solutions" $ do
    properties
      (Solution.completeBinaryTree, Solution.isCompleteBinaryTree)
      ("completeBinaryTree", "isCompleteBinaryTree")

stripValue :: Tree a -> Tree ()
stripValue Empty          = Empty
stripValue (Branch _ l r) = Branch () (stripValue l) (stripValue r)

-- | For small non-negative integers to prevent combinatorial explosion.
newtype Tiny = Tiny Int deriving (Show)

instance Arbitrary Tiny where
  arbitrary = Tiny <$> resize 15 arbitrarySizedNatural
  shrink (Tiny n) = map Tiny $ shrink n
