module Problems.P71Spec (spec) where

import           Problems.MultiwayTrees
import           Problems.MultiwayTrees.Arbitrary ()
import qualified Problems.P71                     as Problem
import qualified Solutions.P71                    as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: (MultiwayTree Int -> Int) -> String -> Spec
properties internalPathLength name = do
  describe name $ do
    prop "is sum of all path lengths starting from root" $
      \t -> internalPathLength t `shouldBe` sum (map pathLength $ pathsFromRoot t)

examples :: Spec
examples = do
  describe "Examples" $ do
    it "internalPathLength multitree5" $ do
      internalPathLength multitree5 `shouldBe` 9

    it "internalPathLength multitree4" $ do
      internalPathLength multitree4 `shouldBe` 2

  where internalPathLength = Problem.internalPathLength

spec :: Spec
spec = do
  properties Problem.internalPathLength "internalPathLength"
  examples
  describe "From solutions" $ do
    properties Solution.internalPathLength "internalPathLength"

pathLength :: [a] -> Int
pathLength path = length path - 1

pathsFromRoot :: MultiwayTree a -> [[a]]
pathsFromRoot (MultiwayTree x []) = [[x]]
pathsFromRoot (MultiwayTree x ts) = (:) [x] $ concat $ map (map (x:) . pathsFromRoot) ts
