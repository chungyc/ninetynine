{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P57Spec (spec) where

import           Data.List             (group)
import           Problems.BinaryTrees
import           Problems.P56
import qualified Problems.P57          as Problem
import qualified Solutions.P57         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Tree Int) -> String -> (Int -> Tree Int -> Tree Int) -> String -> Spec
properties construct constructName addedTo addedToName = do
  describe constructName $ do
    prop "returns a tree which can be binary searched" $
      \xs -> xs `shouldSatisfy ` all (\x -> search (construct xs) x == Just x)

    prop "does not have elements beyond input list" $
      \xs -> \y ->
        not (y `elem` xs) ==>
        search (construct xs) y `shouldBe` Nothing

    prop "should add elements from front to back of list" $
      \xs -> construct xs `shouldBe` foldl (flip addedTo) Empty xs

  describe addedToName $ do
    prop "returns a tree which can be binary searched" $
      \xs -> \y -> search (y `addedTo` construct xs) y `shouldBe` Just y

    prop "should have same structure as original tree except for single leaf" $
      \xs -> \y ->
        distinct xs ==>
        let tree = construct xs
            -- returns (has same structure except with extra leaves, values in extra leaves)
            match Empty Empty = (True, [])
            match Empty (Branch z Empty Empty) = (True, [z])
            match (Branch z Empty Empty) Empty = (True, [z])
            match (Branch z l r) (Branch z' l' r')
              | z /= z' = (False, [])
              | otherwise = let (b,  zs)  = match l l'
                                (b', zs') = match r r'
                            in (b && b', zs ++ zs')
            match _ _ = (False, [])
            (matched, residue) = match tree (y `addedTo` tree)
        in matched && residue == [y]

  where
    -- binary tree search
    search Empty _ = Nothing
    search (Branch x left right) y
      | x == y    = Just x
      | x < y     = search right y
      | otherwise = search left y

    -- for ignoring lists with duplicate elements
    -- specifying whether to use < or <= to go left is overly restrictive
    distinct xs = not $ any (\ys -> length ys > 1) $ group xs

examples :: Spec
examples = do
  describe "Examples" $ do
    it "construct [3, 2, 5, 7, 1]" $ do
      construct [3, 2, 5, 7, 1 :: Int] `shouldBe`
        Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))

    it "symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]" $ do
      (symmetric . construct) [5, 3, 18, 1, 4, 12, 21 :: Int] `shouldBe` True

    it "symmetric . construct $ [3, 2, 5, 7, 1]" $ do
      (symmetric . construct) [3, 2, 5, 7, 1 :: Int] `shouldBe` True

    it "8 `addedTo` Branch 5 Empty (Branch 7 Empty (Branch 9 Empty Empty))" $ do
      (8 :: Int) `addedTo` Branch 5 Empty (Branch 7 Empty (Branch 9 Empty Empty))
        `shouldBe` Branch 5 Empty (Branch 7 Empty (Branch 9 (Branch 8 Empty Empty) Empty))

  where construct xs = Problem.construct xs
        addedTo x t = Problem.addedTo x t

spec :: Spec
spec = parallel $ do
  properties Problem.construct "construct" Problem.addedTo "addedTo"
  examples
  describe "From solutions" $ do
    properties Solution.construct "construct" Solution.addedTo "addedTo"
