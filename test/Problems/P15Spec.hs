module Problems.P15Spec (spec) where

import qualified Problems.P15          as Problem
import qualified Solutions.P15         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Int -> [Int]) -> String -> Spec
properties repli name = do
  describe name $ do
    prop "repeats elements given number of times" $
      let naiveRepli l n = concat $ map (replicate n) l
      in \l -> \(NonNegative n) -> repli l n `shouldBe` naiveRepli l n

examples :: Spec
examples = do
  describe "Examples" $ do
    it "repli \"abc\" 3" $ do
      repli "abc" 3 `shouldBe` "aaabbbccc"

  where repli = Problem.repli

spec :: Spec
spec = parallel $ do
  properties Problem.repli "repli"
  examples
  describe "From solutions" $ do
    properties Solution.repli "repli"
