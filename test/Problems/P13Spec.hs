module Problems.P13Spec (spec) where

import           Problems.Lists
import           Problems.P11
import qualified Problems.P13          as Problem
import qualified Solutions.P13         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> [Encoding Int]) -> String -> Spec
properties encodeDirect name = do
  describe name $ do
    prop "is same as encodeModified" $
      \l -> encodeDirect l `shouldBe` encodeModified (l :: [Int])

examples :: Spec
examples = do
  describe "Examples" $ do
    it "encodeDirect \"aaaabccaadeeee\"" $ do
      encodeDirect "aaaabccaadeeee" `shouldBe`
        [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']

  where encodeDirect l = Problem.encodeDirect l

spec :: Spec
spec = do
  properties Problem.encodeDirect "encodeDirect"
  examples
  describe "From solutions" $ do
    properties Solution.encodeDirect "encodeDirect"
