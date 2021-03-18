module Problems.P63Spec (spec) where

import           Problems.BinaryTrees
import           Problems.BinaryTrees.Arbitrary ()
import qualified Problems.P63                   as Problem
import qualified Solutions.P63                  as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

completeBinaryTreeProp :: (Int -> Tree ()) -> String -> Spec
completeBinaryTreeProp completeBinaryTree name = do
  describe name $ do
    prop "has given number of nodes" $
      \(NonNegative n) -> completeBinaryTree n `shouldSatisfy` (==) n . treeSize

    prop "level order is internal nodes, leaves, then empty" $
      \(NonNegative n) ->
        completeBinaryTree n
        `shouldSatisfy` null . dropWhile isEmpty . dropWhile isLeaf . dropWhile isInternal . levelOrder

isCompleteBinaryTreeProp :: (Tree Int -> Bool) -> String -> Spec
isCompleteBinaryTreeProp isCompleteBinaryTree name = do
  describe name $ do
    prop "if and only if level order is consistent with complete binary tree" $
      \t -> classify (isCompleteBinaryTree t) "complete binary tree" $
            isCompleteBinaryTree t `shouldBe`
            (null $ dropWhile isEmpty $ dropWhile isLeaf $ dropWhile isInternal $ levelOrder t)

examples :: Spec
examples = do
  describe "Examples" $ do
    it "completeBinaryTree 4" $ do
      completeBinaryTree 4 `shouldBe`
        Branch () (Branch () (Branch () Empty Empty) Empty) (Branch () Empty Empty)

    it "isCompleteBinaryTree $ Branch () (Branch () Empty Empty) Empty" $ do
      (isCompleteBinaryTree $ Branch () (Branch () Empty Empty) Empty) `shouldBe` True

    it "isCompleteBinaryTree $ Branch () Empty (Branch () Empty Empty)" $ do
      (isCompleteBinaryTree $ Branch () Empty (Branch () Empty Empty)) `shouldBe` False

  where completeBinaryTree = Problem.completeBinaryTree
        isCompleteBinaryTree = Problem.isCompleteBinaryTree

spec :: Spec
spec = parallel $ do
  completeBinaryTreeProp Problem.completeBinaryTree "completeBinaryTree"
  isCompleteBinaryTreeProp Problem.isCompleteBinaryTree "isCompleteBinaryTree"
  examples
  describe "From solutions" $ do
    completeBinaryTreeProp Solution.completeBinaryTree "completeBinaryTree"
    isCompleteBinaryTreeProp Solution.isCompleteBinaryTree "isCompleteBinaryTree"

levelOrder :: Tree a -> [Tree a]
levelOrder t = concat $ takeWhile (not . null) $ map (atLevel t) [1..]

-- Nodes from each level, including empty nodes.
atLevel :: Tree a -> Int -> [Tree a]
atLevel t 1              = [t]
atLevel Empty _          = []
atLevel (Branch _ l r) n = atLevel l (n-1) ++ atLevel r (n-1)

isInternal :: Tree a -> Bool
isInternal (Branch _ Empty Empty) = False
isInternal (Branch _ _ _)         = True
isInternal _                      = False

isLeaf :: Tree a -> Bool
isLeaf (Branch _ Empty Empty) = True
isLeaf _                      = False

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False
