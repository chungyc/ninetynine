module Problems.P09Spec (spec) where

import           Problems.P08
import qualified Problems.P09          as Problem
import qualified Solutions.P09         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> [[Int]]) -> String -> Spec
properties pack name = do
  describe name $ do
    prop "places identical elements in a sublist" $
      let repeated []           = True
          repeated [_]          = True
          repeated (x:ys@(y:_)) = x == y && repeated ys
      in \l -> classify (l == compress l) "trivial" $
               pack l `shouldSatisfy` all repeated

    prop "has expected sublist for each consecutive segment" $
      \l -> classify (l == compress l) "trivial" $
            map head (pack l) `shouldBe` compress l

    prop "has the same elements but in sublists" $
      \l -> classify (l == compress l) "trivial" $
            (concat .  pack) l `shouldBe` l

examples :: Spec
examples = do
  describe "Examples" $ do
    it "pack \"aaaabccaadeeee\"" $ do
      pack "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"]

  where pack l = Problem.pack l

spec :: Spec
spec = do
  properties Problem.pack "pack"
  examples
  describe "From solutions" $ do
    properties Solution.pack  "pack"
    properties Solution.pack' "pack'"
