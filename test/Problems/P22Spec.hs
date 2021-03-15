module Problems.P22Spec (spec) where

import           Data.List             (nub)
import qualified Problems.P22          as Problem
import qualified Solutions.P22         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> Int -> [Int]) -> String -> Spec
properties range name = do
  describe name $ do
    prop "has numbers within range" $
      \m -> \(NonNegative k) -> range m (m+k) `shouldSatisfy` all (\x -> m <= x && x <= m+k)

    prop "has distinct numbers" $
      \m -> \(NonNegative k) -> range m (m+k) `shouldSatisfy` (\l -> nub l == l)

    prop "has expected number of numbers" $
      \m -> \(NonNegative k) -> range m (m+k) `shouldSatisfy` (==) (k+1) . length

examples :: Spec
examples = do
  describe "Examples" $ do
    it "range 4 9" $ do
      range 4 9 `shouldMatchList` [4,5,6,7,8,9]

  where range = Problem.range

spec :: Spec
spec = do
  properties Problem.range "range"
  examples
  describe "From solutions" $ do
    properties Solution.range "range"
