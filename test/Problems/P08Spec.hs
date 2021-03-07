module Problems.P08Spec (spec) where

import qualified Problems.P08          as Problem
import qualified Solutions.P08         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> [Int]) -> String -> Spec
properties compress name = do
  describe name $ do
    prop "leaves no consecutive duplicates" $
      let consecutive []           = False
          consecutive [_]          = False
          consecutive (x:xs@(y:_)) = x == y || consecutive xs
      in \l -> classify (not . consecutive $ l) "trivial" $
               compress l `shouldSatisfy` not . consecutive

    prop "leaves elements in same order" $
      let consume _ [] = []
          consume x (y:ys)
            | x == y    = consume x ys
            | otherwise = (y:ys)
          sameOrder ([], []) = True
          sameOrder ([], _)  = False
          sameOrder (_, [])  = False
          sameOrder (x:xs, y:ys)
            | x == y    = sameOrder (consume x xs, consume y ys)
            | otherwise = False
      in \l -> (l, compress l) `shouldSatisfy` sameOrder

examples :: Spec
examples = do
  describe "Examples" $ do
    it "compress \"aaaabccaadeeee\"" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"

  where compress l = Problem.compress l

spec :: Spec
spec = parallel $ do
  properties Problem.compress "compress"
  examples
  describe "From solutions" $ do
    properties Solution.compress "compress"
