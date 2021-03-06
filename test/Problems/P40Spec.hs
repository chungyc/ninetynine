module Problems.P40Spec (spec) where

import           Problems.P31
import qualified Problems.P40          as Problem
import qualified Solutions.P40         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> (Integer, Integer)) -> String -> Spec
properties goldbach name = do
  describe name $ do
    prop "are prime numbers summing up to given even number" $
      \(Positive n') ->
        let n = 2*(n'+1)  -- even number larger than 2
            (p,q) = goldbach n
        in conjoin [ p+q `shouldBe` n
                   , p `shouldSatisfy` isPrime
                   , q `shouldSatisfy` isPrime
                   ]

examples :: Spec
examples = do
  describe "Examples" $ do
    it "goldbach 12" $ do
      goldbach 12 `shouldBe` (5,7)

  where goldbach n = Problem.goldbach (n :: Integer)

spec :: Spec
spec = do
  properties Problem.goldbach "goldbach"
  examples
  describe "From solutions" $ do
    properties Solution.goldbach "goldbach"

