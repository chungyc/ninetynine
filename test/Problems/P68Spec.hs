{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P68Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary  ()
import           Problems.BinaryTrees.QuickCheck
import           Problems.P54
import qualified Problems.P68                    as Problem
import qualified Solutions.P68                   as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties
  :: (Tree Int -> [Int], Tree Int -> [Int], [Int] -> [Int] -> Tree Int)
  -> (String, String, String)
  -> Spec
properties
  (inorder, preorder, ordersToTree)
  (nameInorder, namePreorder, nameOrdersToTree) = do
  describe nameInorder $ do
    prop "empty tree has empty in-order sequence" $ do
      inorder Empty `shouldBe` []

    prop "is in-order sequence" $ \x t t' ->
      inorder (Branch x t t') `shouldBe` inorder t ++ [x] ++ inorder t'

  describe namePreorder $ do
    prop "empty tree has empty pre-order sequence" $ do
      preorder Empty `shouldBe` []

    prop "is pre-order sequence" $ \x t t' ->
      preorder (Branch x t t') `shouldBe` [x] ++ preorder t ++ preorder t'

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
  properties
    (Problem.inorder, Problem.preorder, Problem.ordersToTree)
    ("inorder", "preorder", "ordersToTree")
  examples
  describe "From solutions" $ do
    properties
      (Solution.inorder, Solution.preorder, Solution.ordersToTree)
      ("inorder", "preorder", "ordersToTree")

-- | Arbitrary binary tree with distinct node values.
newtype DistinctTree = DistinctTree (Tree Int) deriving Show

instance Arbitrary DistinctTree where
  arbitrary = DistinctTree <$> do
    k <- getSize
    xs <- shuffle [0..(32*k)]  -- Try to use plenty of distinct values.
    gen k xs
    where gen _ []     = empty
          gen 0 (x:xs) = frequency [ (10, empty), (1, tree x xs) ]
          gen _ (x:xs) = frequency [ (1, empty), (10, tree x xs) ]

          empty = pure Empty

          tree x xs = Branch x <$> subtree xs' <*> subtree xs''
            where subtree ys = sized $ \k -> scale (`div` 2) $ gen k ys
                  (xs', xs'') = split xs

                  split [] = ([], [])
                  split [y] = ([y], [])
                  split (y:y':ys) = (y:zs, y':zs')
                    where (zs, zs') = split ys

  shrink (DistinctTree t) = map DistinctTree $ shrinkTree shrinkNothing t
