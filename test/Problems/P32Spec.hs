module Problems.P32Spec (spec) where

import qualified Problems.P32          as Problem
import qualified Solutions.P32         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Integer -> Integer) -> String -> Spec
properties myGCD name = do
  describe name $ do
    prop "divides both integers" $
      \(NonZero a) -> \(NonZero b) ->
        myGCD a b `shouldSatisfy` \c -> a `mod` c == 0 && b `mod` c == 0

    prop "has no greater common divisor" $
      \(NonZero a) -> \(NonZero b) ->
        [(1 + myGCD a b)..(min (abs a) (abs b))]
        `shouldSatisfy` all (\k -> a `mod` k /= 0 || b `mod` k /= 0)

examples :: Spec
examples = do
  describe "Examples" $ do
    it "[myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]" $ do
      [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6] `shouldBe` [9, 3, 3 :: Int]

  where myGCD a b = Problem.myGCD a b

spec :: Spec
spec = do
  properties Problem.myGCD "myGCD"
  examples
  describe "From solutions" $ do
    properties Solution.myGCD "myGCD"
