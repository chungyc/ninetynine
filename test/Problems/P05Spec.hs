module Problems.P05Spec (spec) where

import qualified Problems.P05          as Problem
import qualified Solutions.P05         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> [Int]) -> String -> Spec
properties myReverse name = do
  describe name $ do
    prop "reverses a list" $
      \l -> let naiveReverse []     = []
                naiveReverse (x:xs) = (naiveReverse xs) ++ [x]
            in myReverse l `shouldBe` naiveReverse l

    prop "returns original from reversed list" $
      \l -> myReverse (myReverse l) `shouldBe` l

examples :: Spec
examples =
  describe "Examples" $ do
    it "myReverse \"A man, a plan, a canal, panama!\"" $ do
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"

    it "myReverse [1,2,3,4]" $ do
      myReverse [1,2,3,4] `shouldBe` [4,3,2,1 :: Int]

  where myReverse = Problem.myReverse

spec :: Spec
spec = do
  properties Problem.myReverse "myReverse"
  examples
  describe "From solutions" $ do
    properties Solution.myReverse "myReverse"
