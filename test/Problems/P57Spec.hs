{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P57Spec (spec) where

import           Problems.BinaryTrees
import           Problems.P56
import qualified Problems.P57          as Problem
import qualified Solutions.P57         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties
  :: ([Int] -> Tree Int) -> String
  -> (Int -> Tree Int -> Tree Int) -> String
  -> Spec
properties construct constructName addedTo addedToName = do
  describe constructName $ do
    prop "returns a tree whose elements can be searched" $
      \(NonEmpty xs) -> forAll (elements xs) $ \x ->
        construct xs `shouldSatisfy ` (x `isContainedBy`)

    prop "does not have elements beyond input list" $
      \xs -> \x -> not (x `elem` xs) ==>
      construct xs `shouldNotSatisfy` (x `isContainedBy`)

    prop "should add elements from front to back of list" $
      \xs -> construct xs `shouldBe` foldl (flip addedTo) Empty xs

  describe addedToName $ do
    prop "returns a tree from which the element can be searched" $
      \(SearchTree t) -> \x ->
        x `addedTo` t `shouldSatisfy` (x `isContainedBy`)

    prop "previous elements should still be searchable" $
      \(NonEmpty xs) -> forAll (elements xs) $ \x -> \y ->
        y `addedTo` (construct xs) `shouldSatisfy` (x `isContainedBy`)

    prop "does not add extra elements" $
      \(SearchTree t) -> \x -> \y ->
        x /= y ==>
        not (y `isContainedBy` t) ==>
        x `addedTo` t `shouldNotSatisfy` (y `isContainedBy`)

    prop "is same as input tree except for new leaf" $
      \(SearchTree t) -> \x ->
        x `addedTo` t `shouldSatisfy` (==) [(Empty, Branch x Empty Empty)] . diff t

examples :: Spec
examples = describe "Examples" $ do
  it "construct [3, 2, 5, 7, 1]" $ do
    construct [3, 2, 5, 7, 1] `shouldBe`
      Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))

  it "symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]" $ do
    (symmetric . construct) [5, 3, 18, 1, 4, 12, 21] `shouldBe` True

  it "symmetric . construct $ [3, 2, 5, 7, 1]" $ do
    (symmetric . construct) [3, 2, 5, 7, 1] `shouldBe` True

  it "8 `addedTo` Branch 5 Empty (Branch 7 Empty (Branch 9 Empty Empty))" $ do
    8 `addedTo` Branch 5 Empty (Branch 7 Empty (Branch 9 Empty Empty))
      `shouldBe` Branch 5 Empty (Branch 7 Empty (Branch 9 (Branch 8 Empty Empty) Empty))

  where construct xs = Problem.construct xs :: Tree Int
        addedTo x t = Problem.addedTo x t :: Tree Int

spec :: Spec
spec = parallel $ do
  properties Problem.construct "construct" Problem.addedTo "addedTo"
  examples
  describe "From solutions" $ do
    properties Solution.construct "construct" Solution.addedTo "addedTo"

-- | Binary tree search.
search :: Ord a => a -> Tree a -> Maybe a
search _ Empty = Nothing
search x (Branch y left right)
  | x == y    = Just x
  | x < y     = search x left
  | otherwise = search x right

-- | Whether a value is contained by a binary search tree.
isContainedBy :: Ord a => a -> Tree a -> Bool
isContainedBy x t | Nothing <- found = False
                  | Just _ <- found = True
  where found = search x t

-- | Gather the differences between two trees.
diff :: Eq a => Tree a -> Tree a -> [(Tree a, Tree a)]
diff Empty Empty = []
diff t@(Branch _ _ _) Empty = [(t, Empty)]
diff Empty t@(Branch _ _ _) = [(Empty, t)]
diff t@(Branch x l r) t'@(Branch x' l' r')
  | x /= x'= [(t, t')]
  | otherwise = diff l l' ++ diff r r'

-- | A binary search tree.
newtype SearchTree a = SearchTree (Tree a) deriving (Show)

instance (Arbitrary a, Ord a) => Arbitrary (SearchTree a) where
  arbitrary = SearchTree . Solution.construct <$> arbitrary

  shrink (SearchTree Empty) = []
  shrink (SearchTree (Branch x l r)) =
    -- The subtrees are binary search trees, too.
    [ SearchTree Empty, SearchTree l, SearchTree r ] ++
    -- As long as the values are not changed,
    -- the shrunken trees are still binary search trees.
    [ SearchTree $ Branch x l' r' | (SearchTree l') <- shrink (SearchTree l)
                                  , (SearchTree r') <- shrink (SearchTree r) ]
