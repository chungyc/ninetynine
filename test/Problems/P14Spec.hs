module Problems.P14Spec (spec) where

import qualified Problems.P14          as Problem
import qualified Solutions.P14         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> [Int]) -> String -> Spec
properties dupli name = do
  describe name $ do
    prop "duplicates elements of list" $
      let doubled [] []            = True
          doubled (x:xs) (y:y':ys) = x == y && y == y' && doubled xs ys
          doubled _ _              = False
      in \l -> dupli l `shouldSatisfy` doubled (l :: [Int])

examples :: Spec
examples = do
  describe "Examples" $ do
    it "dupli [1, 2, 3]" $ do
      dupli [1, 2, 3] `shouldBe` [1,1,2,2,3,3 :: Int]

  where dupli = Problem.dupli

spec :: Spec
spec = parallel $ do
  properties Problem.dupli "dupli"
  examples
  describe "From solutions" $ do
    properties Solution.dupli "dupli"
