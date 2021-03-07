module Problems.P18Spec (spec) where

import qualified Problems.P18          as Problem
import qualified Solutions.P18         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Int -> Int -> [Int]) -> String -> Spec
properties slice name = do
  describe name $ do
    prop "extracts slice" $
      \(Positive a) -> \(NonNegative b) -> \(NonNegative c) ->
        slice [1..a+b+c] a (a+b) `shouldBe` [a..a+b]

examples :: Spec
examples = do
  describe "Examples" $ do
    it "slice ['a','b','c','d','e','f','g','h','i','k'] 3 7" $ do
      slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"

  where slice = Problem.slice

spec :: Spec
spec = parallel $ do
  properties Problem.slice "slice"
  examples
  describe "From solutions" $ do
    properties Solution.slice  "slice"
    properties Solution.slice' "slice'"
