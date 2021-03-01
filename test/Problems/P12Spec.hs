module Problems.P12Spec (spec) where

import           Problems.Lists
import           Problems.P11
import qualified Problems.P12          as Problem
import qualified Solutions.P12         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Encoding Int] -> [Int]) -> String -> Spec
properties decodeModified name = do
  describe name $ do
    prop "restores original list from its encoding" $
      \l -> decodeModified (encodeModified l) `shouldBe` l

examples :: Spec
examples = do
  describe "Examples" $ do
    it "decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']" $ do
      decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
        `shouldBe` "aaaabccaadeeee"

  where decodeModified = Problem.decodeModified

spec :: Spec
spec = do
  properties Problem.decodeModified "decodeModified"
  examples
  describe "From solutions" $ do
    properties Solution.decodeModified  "decodeModified"
    properties Solution.decodeModified' "decodeModified'"
