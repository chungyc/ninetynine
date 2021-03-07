module Problems.P16Spec (spec) where

import qualified Problems.P16          as Problem
import qualified Solutions.P16         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Int -> [Int]) -> String -> Spec
properties dropEvery name = do
  describe name $ do
    prop "has every N'th element missing from original list" $
      \(Positive n) -> \l ->
        let scan _ [] [] = True
            scan _ [] _ = False
            scan k (_:xs) []
              | k `mod` n == 0 = scan (k+1) xs []
              | otherwise      = False
            scan k (x:xs) ys'@(y:ys)
              | k `mod` n == 0 = scan (k+1) xs ys'
              | x == y         = scan (k+1) xs ys
              | otherwise      = True
        in dropEvery l n `shouldSatisfy` scan 1 l

examples :: Spec
examples = do
  describe "Examples" $ do
    it "dropEvery \"abcdefghik\" 3" $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

  where dropEvery = Problem.dropEvery

spec :: Spec
spec = parallel $ do
  properties Problem.dropEvery "dropEvery"
  examples
  describe "From solutions" $ do
    properties Solution.dropEvery "dropEvery"
