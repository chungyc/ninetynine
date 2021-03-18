module Problems.P64Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import qualified Problems.P64                   as Problem
import qualified Solutions.P64                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: (Tree Char -> Tree (Char, (Int, Int))) -> String -> Spec
properties layoutInorder name = do
  describe name $ do
    prop "horizontal location is position in in-order sequence" $
      \t -> layoutInorder t `shouldSatisfy` (==) [1..(treeSize t)] . inorderSequence

    prop "vertical location is depth" $
      \t -> layoutInorder t `shouldSatisfy` verticalIsDepth

    prop "keeps original values" $
      \t -> layoutInorder t `shouldSatisfy` hasSameValuesAs t

examples :: Spec
examples = do
  describe "Examples" $ do
    it "layoutInorder tree64" $ do
      layoutInorder tree64 `shouldBe`
        Branch ('n',(8,1))
        (Branch ('k',(6,2))
          (Branch ('c',(2,3))
            (Branch ('a',(1,4)) Empty Empty)
            (Branch ('h',(5,4)) (Branch ('g',(4,5)) (Branch ('e',(3,6)) Empty Empty) Empty) Empty))
          (Branch ('m',(7,3)) Empty Empty))
        (Branch ('u',(12,2)) (Branch ('p',(9,3)) Empty
                              (Branch ('s',(11,4)) (Branch ('q',(10,5)) Empty Empty) Empty)) Empty)

  where layoutInorder = Problem.layoutInorder
        tree64 = Problem.tree64

spec :: Spec
spec = parallel $ do
  properties Problem.layoutInorder "layoutInorder"
  examples
  describe "From solutions" $ do
    properties Solution.layoutInorder "layoutInorder"

inorderSequence :: Tree (a, (Int, Int)) -> [Int]
inorderSequence Empty                    = []
inorderSequence (Branch (_, (x, _)) l r) = inorderSequence l ++ [x] ++ inorderSequence r

verticalIsDepth :: Tree (a, (Int, Int)) -> Bool
verticalIsDepth t = verticalIsDepth' t 1

verticalIsDepth' :: Tree (a, (Int, Int)) -> Int -> Bool
verticalIsDepth' Empty _ = True
verticalIsDepth' (Branch (_, (_, y)) l r) d =
  d == y && verticalIsDepth' l (d+1) && verticalIsDepth' r (d+1)

hasSameValuesAs :: Eq a => Tree a -> Tree (a, (Int, Int)) -> Bool
hasSameValuesAs Empty Empty = True
hasSameValuesAs (Branch x l r) (Branch (x', _) l' r') =
  x == x' && hasSameValuesAs l l' && hasSameValuesAs r r'
hasSameValuesAs _ _ = False
