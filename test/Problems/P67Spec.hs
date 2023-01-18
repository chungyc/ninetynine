{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P67Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.QuickCheck
import qualified Problems.P67                    as Problem
import qualified Solutions.P67                   as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Tree Char -> String, String -> Tree Char) -> (String, String) -> Spec
properties (treeToString, stringToTree) (nameTreeToString, nameStringToTree) = do
  describe nameTreeToString $ do
    prop "is empty string for empty tree" $
      treeToString Empty `shouldBe` ""

    prop "is single character for leaf" $
      forAll (elements ['a'..'z']) $ \c ->
      treeToString (Branch c Empty Empty) `shouldBe` [c]

    prop "is node value and subtrees in parentheses" $ \(CharTree t) (CharTree t') ->
      forAll letters $ \c ->
      t /= Empty || t' /= Empty ==>
      treeToString (Branch c t t') `shouldBe`
      [c] ++ "(" ++ treeToString t ++ "," ++ treeToString t' ++ ")"

  describe nameStringToTree $ do
    prop "is inverse of treeToString" $ \(CharTree t) ->
      (stringToTree . treeToString) t `shouldBe` t

examples :: Spec
examples = describe "Examples" $ do
  it "treeToString $ Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))" $ do
    treeToString (Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty)))
      `shouldBe` "x(y,a(,b))"

  it "stringToTree \"x(y,a(,b))\"" $ do
    stringToTree "x(y,a(,b))" `shouldBe`
      Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))

  where treeToString = Problem.treeToString
        stringToTree = Problem.stringToTree

spec :: Spec
spec = parallel $ do
  properties (Problem.treeToString, Problem.stringToTree) ("treeToString", "stringToTree")
  examples
  describe "From solutions" $ do
    properties (Solution.treeToString, Solution.stringToTree) ("treeToString", "stringToTree")

-- | Generates a letter.
--
-- It will not generate the special characters '.', '(', and ')'.
letters :: Gen Char
letters = choose ('a', 'z')

-- | Arbitrary tree with character values.
--
-- It should not have the special characters '.', '(', and ')'.
newtype CharTree = CharTree (Tree Char) deriving Show

instance Arbitrary CharTree where
  arbitrary = CharTree <$> treesOf letters
  shrink (CharTree t) = map CharTree $ shrinkTree shrinkNothing t
