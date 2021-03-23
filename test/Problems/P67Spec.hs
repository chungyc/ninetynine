module Problems.P67Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import qualified Problems.P67                   as Problem
import qualified Solutions.P67                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Tree Char -> String, String -> Tree Char) -> (String, String) -> Spec
properties (treeToString, stringToTree) (nameTreeToString, nameStringToTree) = do
  describe nameTreeToString $ do
    it "is empty string for empty tree" $ do
      treeToString Empty `shouldBe` ""

    prop "is single character for leaf" $ do
      forAll (elements ['a'..'z']) $
        \c -> treeToString (Branch c Empty Empty) `shouldBe` [c]

    prop "is node value and parenthesized subtrees for internal nodes" $
      withTree $ \t@(~(Branch c l r)) ->
      isInternalNode t ==>
      treeToString t `shouldBe` [c] ++ "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"

    prop "is inverse of stringToTree" $
      withString $ \s -> (treeToString . stringToTree) s `shouldBe` s

  describe nameStringToTree $ do
    prop "is inverse of treeToString" $
      withTree $ \t -> (stringToTree . treeToString) t `shouldBe` t

  where
    -- Exclude special characters.
    withTree f = \t -> counterexample (show $ replaceSpecial t) $ f $ replaceSpecial t
    replaceSpecial Empty = Empty
    replaceSpecial (Branch c l r)
      | c `elem` ['(', ')', ','] = Branch 'x' (replaceSpecial l) (replaceSpecial r)
      | otherwise                = Branch c (replaceSpecial l) (replaceSpecial r)

    -- Restrict strings to those representing binary trees
    -- with restricted set of characters.
    withString f = \t -> let s = treeToString $ replaceSpecial t in counterexample s $ f s

    isInternalNode Empty                  = False
    isInternalNode (Branch _ Empty Empty) = False
    isInternalNode _                      = True

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
