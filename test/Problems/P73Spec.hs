{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P73Spec (spec) where

import           Data.List                         (intercalate)
import           Problems.MultiwayTrees
import           Problems.MultiwayTrees.QuickCheck
import qualified Problems.P73                      as Problem
import qualified Solutions.P73                     as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties
  :: (MultiwayTree Char -> String, String -> MultiwayTree Char)
  -> (String, String)
  -> Spec
properties
  (treeToSexp, sexpToTree)
  (nameTreeToSexp, nameSexpToTree) = do
  describe nameTreeToSexp $ do
    prop "is bare for tree with no children" $
      forAll letters $ \x ->
      treeToSexp (MultiwayTree x []) `shouldBe` [x]

    prop "is list of node value and subtrees" $
      \(CharTree t@(MultiwayTree x ts)) ->
        not (null ts) ==>
        treeToSexp t `shouldBe` "(" ++ (intercalate " " $ [x] : map treeToSexp ts) ++ ")"

  describe nameSexpToTree $ do
    prop "is inverse of treeToSexp" $
      \(CharTree t) -> (sexpToTree . treeToSexp) t `shouldBe` t

examples :: Spec
examples = describe "Examples" $ do
  it "treeToSexp multitree1" $ do
    treeToSexp multitree1 `shouldBe` "a"

  it "treeToSexp multitree2" $ do
    treeToSexp multitree2 `shouldBe` "(a b)"

  it "treeToSexp multitree3" $ do
    treeToSexp multitree3 `shouldBe` "(a (b c))"

  it "treeToSexp multitree4" $ do
    treeToSexp multitree4 `shouldBe` "(b d e)"

  it "treeToSexp multitree5" $ do
    treeToSexp multitree5 `shouldBe` "(a (f g) c (b d e))"

  it "sexpToTree \"(a (f g) c (b d e))\"" $ do
    sexpToTree "(a (f g) c (b d e))" `shouldBe` multitree5

  where treeToSexp = Problem.treeToSexp
        sexpToTree = Problem.sexpToTree

spec :: Spec
spec = parallel $ do
  properties (Problem.treeToSexp, Problem.sexpToTree) ("treeToSexp", "sexpToTree")
  examples
  describe "From solutions" $ do
    properties (Solution.treeToSexp, Solution.sexpToTree) ("treeToSexp", "sexpToTree")

-- | Generate letters.
--
-- It will not generate the special characters ' ', '(', and ')'.
letters :: Gen Char
letters = choose ('a', 'z')

-- | A multiway tree with character values.
--
-- It should not have the special characters ' ', '(', and ')'.
newtype CharTree = CharTree (MultiwayTree Char) deriving (Show)

instance Arbitrary CharTree where
  arbitrary = CharTree <$> multiwayTreesOf letters
  shrink (CharTree t) = map CharTree $ shrinkMultiwayTree shrinkNothing t
