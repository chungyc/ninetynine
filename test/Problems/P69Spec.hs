{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P69Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.QuickCheck
import           Problems.P54
import qualified Problems.P69                    as Problem
import qualified Solutions.P69                   as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties
  :: (String -> Tree Char, Tree Char -> String)
  -> (String, String)
  -> Spec
properties
  (dotstringToTree, treeToDotstring)
  (nameDotstringToTree, nameTreeToDotstring) = do
  describe nameDotstringToTree $ do
    prop "is inverse of treeToDotstring" $ \(CharTree t) ->
      (dotstringToTree . treeToDotstring) t `shouldBe` t

  describe nameTreeToDotstring $ do
    prop "is dot for empty tree" $
      treeToDotstring Empty `shouldBe` "."

    prop "is in pre-order" $ \(CharTree l) (CharTree r) ->
      forAll letters $ \c ->
      treeToDotstring (Branch c l r) `shouldBe`
      [c] ++ treeToDotstring l ++ treeToDotstring r

examples :: Spec
examples = describe "Examples" $ do
  it "dotstringToTree \"xy..z0...\"" $ do
    dotstringToTree "xy..z0..." `shouldBe`
      Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty)

  it "treeToDotstring tree1" $ do
    treeToDotstring tree1 `shouldBe` "abd..e..c.fg..."

  where dotstringToTree = Problem.dotstringToTree
        treeToDotstring = Problem.treeToDotstring

spec :: Spec
spec = parallel $ do
  properties
    (Problem.dotstringToTree, Problem.treeToDotstring)
    ("dotstringToTree", "treeToDotstring")
  examples
  describe "From solutions" $ do
    properties
      (Solution.dotstringToTree, Solution.treeToDotstring)
      ("dotstringToTree", "treeToDotstring")

-- | Generates letters.
--
-- It will not generate the special character '.'.
letters :: Gen Char
letters = choose ('a', 'z')

-- | Arbitrary tree with character values.
--
-- It should not have the special character '.'.
newtype CharTree = CharTree (Tree Char) deriving Show

instance Arbitrary CharTree where
  arbitrary = CharTree <$> treesOf letters
  shrink (CharTree t) = map CharTree $ shrinkTree shrinkNothing t
