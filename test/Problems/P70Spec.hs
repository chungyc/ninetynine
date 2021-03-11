module Problems.P70Spec (spec) where

import           Problems.MultiwayTrees
import           Problems.MultiwayTrees.Arbitrary ()
import qualified Problems.P70                     as Problem
import qualified Solutions.P70                    as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: (String -> MultiwayTree Char, MultiwayTree Char -> String) -> (String, String) -> Spec
properties (stringToTree, treeToString) (nameStringToTree, nameTreeToString) = do
  describe nameStringToTree $ do
    prop "maps string to originating tree" $
      withTree $ \t -> stringToTree (toString $ depthFirstSequence t) `shouldBe` t

    prop "is inverse of treeToString" $
      withTree $ \t -> (stringToTree . treeToString) t `shouldBe` t

  describe nameTreeToString $ do
    prop "maps to string according to definition" $
      withTree $ \t -> treeToString t `shouldBe` (toString $ depthFirstSequence t)

    prop "is inverse of stringToTree" $
      withString $ \s -> (treeToString . stringToTree) s `shouldBe` s

  where withTree f = \t -> f $ excludeSpecialCharacter t
        withString f = withTree $ \t -> f $ toString $ depthFirstSequence t

examples :: Spec
examples = do
  describe "Examples" $ do
    it "stringToTree \"afg^^c^bd^e^^^\" == multitree5" $ do
      stringToTree "afg^^c^bd^e^^^" `shouldBe` multitree5

    it "treeToString multitree5" $ do
      treeToString multitree5 `shouldBe` "afg^^c^bd^e^^^"

  where stringToTree = Problem.stringToTree
        treeToString = Problem.treeToString

spec :: Spec
spec = parallel $ do
  properties (Problem.stringToTree, Problem.treeToString) ("stringToTree", "treeToString")
  examples
  describe "From solutions" $ do
    properties (Solution.stringToTree, Solution.treeToString) ("stringToTree", "treeToString")

-- | It is unavoidable that '^' in the string or tree would make the interpretation of a string ambiguous.
-- Avoid the situation in the first place by excluding it and replacing it arbitrarily with '.'.
excludeSpecialCharacter :: MultiwayTree Char -> MultiwayTree Char
excludeSpecialCharacter (MultiwayTree '^' ts) = MultiwayTree '.' $ map excludeSpecialCharacter ts
excludeSpecialCharacter (MultiwayTree x ts) = MultiwayTree x $ map excludeSpecialCharacter ts

-- | A depth-first traversal sequence over a multiway tree.
type Sequence a = [Element a]

-- | The elements in a traversal sequence.
data Element a
  = Node a  -- ^ A node value in the traversal sequence.
  | Down    -- ^ Going down the tree.
  | Up      -- ^ Going up the tree.

-- | Pretty much the definition of depth-first traversal for a multiway tree.
depthFirstSequence :: MultiwayTree a -> Sequence a
depthFirstSequence (MultiwayTree x ts) = Down : (Node x) : concat (map depthFirstSequence ts) ++ [Up]

-- | The definition of how a node string is mapped from the traversal sequence.
toString :: Sequence Char -> String
toString []            = []
toString (Node x : es) = x : toString es
toString (Down : es)   = toString es
toString (Up : es)     = '^' : toString es
