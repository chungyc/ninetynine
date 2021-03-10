module Problems.P95Spec (spec) where

import qualified Problems.P95          as Problem
import qualified Solutions.P95         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> String) -> String -> Spec
properties fullWords name = do
  describe name $ do
    prop "made up of words mapping back to original integer" $
      \(NonNegative n) -> translate (split $ fullWords n) 0 `shouldBe` n

examples :: Spec
examples = do
  describe "Examples" $ do
    it "fullWords 175" $ do
      fullWords (175 :: Int) `shouldBe` "one-seven-five"

  where fullWords n = Problem.fullWords n

spec :: Spec
spec = parallel $ do
  properties Problem.fullWords "fullWords"
  examples
  describe "From solutions" $ do
    properties Solution.fullWords "fullWords"

split :: String -> [String]
split s = final $ foldr step ([], []) s
  where step '-' (partial, components) = ([], partial : components)
        step c (partial, components)   = (c : partial, components)
        final (partial, components) = partial : components

translate :: [String] -> Int -> Int
translate [] n          = n
translate (x:xs) n = translate xs (10 * n + digit x)
  where digit "zero"  = 0
        digit "one"   = 1
        digit "two"   = 2
        digit "three" = 3
        digit "four"  = 4
        digit "five"  = 5
        digit "six"   = 6
        digit "seven" = 7
        digit "eight" = 8
        digit "nine"  = 9
        digit _       = undefined
