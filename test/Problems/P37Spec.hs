module Problems.P37Spec (spec) where

import           Data.List             (genericLength)
import           Problems.P33
import qualified Problems.P37          as Problem
import qualified Solutions.P37         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Integer) -> String -> Spec
properties totient' name = do
  describe name $ do
    prop "is count of lesser than or equal numbers that are coprime" $
      \(Positive n) -> totient' n `shouldBe` genericLength (filter (\x -> coprime n x) [1..n])

examples :: Spec
examples = do
  describe "Examples" $ do
    it "totient' 10" $ do
      totient' 10 `shouldBe` (4 :: Int)

  where totient' n = Problem.totient' n

spec :: Spec
spec = do
  properties Problem.totient' "totient'"
  examples
  describe "From solution" $ do
    properties Solution.totient' "totient'"
