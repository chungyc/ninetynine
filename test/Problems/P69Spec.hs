module Problems.P69Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import           Problems.P54
import qualified Problems.P69                   as Problem
import qualified Solutions.P69                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (String -> Tree Char, Tree Char -> String) -> (String, String) -> Spec
properties (dotstringToTree, treeToDotstring) (nameDotstringToTree, nameTreeToDotstring) = do
  describe nameDotstringToTree $ do
    prop "is inverse of treeToDotstring" $
      withTree $ \t -> (dotstringToTree . treeToDotstring) t `shouldBe` t

  describe nameTreeToDotstring $ do
    it "is dot for empty tree" $
      treeToDotstring Empty `shouldBe` "."

    prop "is in pre-order" $
      \c -> c /= '.' ==> withTree $ \l -> withTree $ \r ->
        treeToDotstring (Branch c l r) `shouldBe` [c] ++ treeToDotstring l ++ treeToDotstring r

    prop "is inverse of dotstringToTree" $
      withString $ \s -> (treeToDotstring . dotstringToTree) s `shouldBe` s

  where withTree f = \t -> f $ excludeSpecial t
        withString f = \t -> f $ treeToDotstring $ excludeSpecial t
        excludeSpecial Empty = Empty
        excludeSpecial (Branch '.' l r) = Branch 'x' (excludeSpecial l) (excludeSpecial r)
        excludeSpecial (Branch x l r) = Branch x (excludeSpecial l) (excludeSpecial r)

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
  properties (Problem.dotstringToTree, Problem.treeToDotstring) ("dotstringToTree", "treeToDotstring")
  examples
  describe "From solutions" $ do
    properties (Solution.dotstringToTree, Solution.treeToDotstring) ("dotstringToTree", "treeToDotstring")
