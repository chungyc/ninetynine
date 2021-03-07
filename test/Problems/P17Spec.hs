module Problems.P17Spec (spec) where

import qualified Problems.P17          as Problem
import qualified Solutions.P17         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Int -> ([Int], [Int])) -> String -> Spec
properties split name = do
  describe name $ do
    prop "joins back to original list" $
      \l -> forAll (choose (1, length l)) $ \n ->
        let join (a, b) = a ++ b
        in join (split l n) `shouldBe` l

    prop "first part should be expected size" $
      \l  -> forAll (choose (1, length l)) $ \n ->
        n <= length l ==>
        length (fst $ split l n) `shouldBe` n

examples :: Spec
examples = do
  describe "Examples" $ do
    it "split \"abcdefghik\" 3" $ do
      split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

  where split = Problem.split

spec :: Spec
spec = parallel $ do
  properties Problem.split "split"
  examples
  describe "From solutions" $ do
    properties Solution.split "split"
