{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P54Spec (spec) where

import           Problems.BinaryTrees
import qualified Problems.P54         as Problem
import qualified Solutions.P54        as Solution
import           Test.Hspec

data Functions a = Functions { getLeaf  :: a -> Tree a
                             , getTree1 :: Tree a
                             , getTree2 :: Tree a
                             , getTree3 :: Tree a
                             }

charSpec :: Functions Char -> Spec
charSpec fs = do
  describe "leaf" $ do
    it "returns a leaf node" $ do
      leaf 't' `shouldBe` Branch 't' Empty Empty

  describe "tree1" $ do
    it "is defined correctly" $ do
      tree1 `shouldBe`
        Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))
  describe "tree2" $ do
    it "is defined correctly" $ do
      tree2 `shouldBe` Branch 'a' Empty Empty

  describe "tree3" $ do
    it "is defined correctly" $ do
      tree3 `shouldBe` Empty

  where Functions { getLeaf = leaf
                  , getTree1 = tree1
                  , getTree2 = tree2
                  , getTree3 = tree3
                  } = fs

intSpec :: Tree Int -> Spec
intSpec tree4 = describe "tree4" $ do
  it "is defined correctly" $ do
    tree4 `shouldBe`
      Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
               (Branch 2 Empty Empty)

spec :: Spec
spec = parallel $ do
  charSpec $ Functions { getLeaf = Problem.leaf
                       , getTree1 = Problem.tree1
                       , getTree2 = Problem.tree2
                       , getTree3 = Problem.tree3
                       }
  intSpec Problem.tree4

  describe "From solutions" $ do
    charSpec $ Functions { getLeaf = Solution.leaf
                         , getTree1 = Solution.tree1
                         , getTree2 = Solution.tree2
                         , getTree3 = Solution.tree3
                         }
    intSpec Problem.tree4
