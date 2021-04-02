{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P28Spec (spec) where

import           Data.List             (sort)
import           Data.Map.Lazy         ((!))
import qualified Data.Map.Lazy         as Map
import qualified Problems.P28          as Problem
import qualified Solutions.P28         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

lsortProp :: ([[Int]] -> [[Int]]) -> String -> Spec
lsortProp lsort name = describe name $ do
  prop "is permutation of original list" $
    \xs -> lsort xs `shouldSatisfy` (==) (sort xs) . sort

  prop "is sorted by length" $
    \xs -> lsort xs `shouldSatisfy` compareNeighbors (\(x,y) -> length x <= length y)

lfsortProp :: ([[Int]] -> [[Int]]) -> String -> Spec
lfsortProp lfsort name = describe name $ do
  prop "is permutation of original list" $
    \xs -> lfsort xs `shouldSatisfy` (==) (sort xs) . sort

  prop "is sorted by frequency of length" $
    \xs -> lfsort xs `shouldSatisfy` compareNeighbors (\(x,y) -> x <= y) . mapToLengthFrequency

examples :: Spec
examples = describe "Examples" $ do
  it "lsort [\"xxx\",\"xx\",\"xxx\",\"xx\",\"xxxx\",\"xx\",\"x\"]" $ do
    lsort ["xxx","xx","xxx","xx","xxxx","xx","x"] `shouldBe` ["x","xx","xx","xx","xxx","xxx","xxxx"]

  it "lfsort [\"xxx\",\"xx\",\"xxx\",\"xx\",\"xxxx\",\"xx\"]" $ do
    lfsort ["xxx","xx","xxx","xx","xxxx","xx"] `shouldBe` ["xxxx","xxx","xxx","xx","xx","xx"]

  where lsort = Problem.lsort
        lfsort = Problem.lfsort

spec :: Spec
spec = parallel $ do
  lsortProp Problem.lsort "lsort"
  lfsortProp Problem.lfsort "lfsort"
  examples
  describe "From solutions" $ do
    lsortProp Solution.lsort "lsort"
    lfsortProp Solution.lfsort "lfsort"

compareNeighbors :: Ord a => ((a,a) -> Bool) -> [a] -> Bool
compareNeighbors _ []  = True
compareNeighbors _ [_] = True
compareNeighbors f xs  = all f neighbors
  where neighbors = zip xs $ tail xs

mapToLengthFrequency :: [[a]] -> [Int]
mapToLengthFrequency xs = map ((!) freqs . length) xs
  where freqs = Map.fromListWith (+) $ map (\ys -> (length ys, 1)) xs
