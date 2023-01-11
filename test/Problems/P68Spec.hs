{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P68Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import           Problems.P54
import qualified Problems.P68                   as Problem
import qualified Solutions.P68                  as Solution
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
    it "empty tree has empty in-order sequence" $ do
      inorder Empty `shouldBe` []

    prop "is in-order sequence" $ \x -> \t -> \t' ->
      inorder (Branch x t t') `shouldBe` inorder t ++ [x] ++ inorder t'

  describe namePreorder $ do
    it "empty tree has empty pre-order sequence" $ do
      preorder Empty `shouldBe` []

    prop "is pre-order sequence" $ \x -> \t -> \t' ->
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

-- | Binary tree with distinct node values.
newtype DistinctTree = DistinctTree (Tree Int) deriving (Show)

instance Arbitrary DistinctTree where
  arbitrary = DistinctTree <$> do
    k <- getSize
    xs <- shuffle [0..max 1000 (2*k)]
    gen k xs
    where gen _ []     = return Empty
          gen 0 (x:xs) =
            frequency [ (10, return Empty)
                      , (1, Branch x <$> gen 0 xs' <*> gen 0 xs'')
                      ]
            where (xs', xs'') = splitAt (length xs `div` 2) xs
          gen n (x:xs) =
            frequency [ (1, return Empty)
                      , (10, Branch x <$> gen (n `div` 2) xs' <*> gen (n `div` 2) xs'')
                      ]
            where (xs', xs'') = splitAt (length xs `div` 2) xs

  shrink (DistinctTree t) = map DistinctTree $ shrink' t
    where shrink' Empty = []
          shrink' (Branch x l r) =
            [ Empty, l, r ] ++
            [ Branch x l' r' | l' <- shrink' l, r' <- shrink' r ]
