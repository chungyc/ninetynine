{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P28Spec (spec) where

import           Data.List             (sort)
import qualified Problems.P28          as Problem
import qualified Solutions.P28         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([[Int]] -> [[Int]], [[Int]] -> [[Int]]) -> (String, String) -> Spec
properties (lsort, lfsort) (lsortName, lfsortName) = do
  describe lsortName $ do
    prop "is permutation of original list" $ \xs ->
      lsort xs `shouldSatisfy` (==) (sort xs) . sort

    prop "is in same order as sorted lengths" $ \xs ->
      lsort xs `shouldSatisfy` (==) (sort $ map length xs) . map length

  describe lfsortName $ do
    prop "is permutation of original list" $ \xs ->
      lfsort xs `shouldSatisfy` (==) (sort xs) . sort

    prop "is in same order as sorted frequencies of lengths" $ \xs ->
      lfsort xs `shouldSatisfy`  (==) (sort $ toFrequencies xs) . toFrequencies

  where toFrequencies xs = map (\ys -> length $ filter ((==) (length ys) . length) xs) xs

{-
lsortProp :: ([[Int]] -> [[Int]]) -> String -> Spec
lsortProp lsort name = describe name $ do
  prop "is permutation of original list" $ \xs ->
    lsort xs `shouldSatisfy` (==) (sort xs) . sort

  prop "is in same order as sorted lengths" $ \xs ->
    lsort xs `shouldSatisfy` (==) (sort $ map length xs) . map length

lfsortProp :: ([[Int]] -> [[Int]]) -> String -> Spec
lfsortProp lfsort name = describe name $ do
  prop "is permutation of original list" $ \xs ->
    lfsort xs `shouldSatisfy` (==) (sort xs) . sort

  prop "is in same order as sorted frequencies of lengths" $ \xs ->
    lfsort xs `shouldSatisfy`  (==) (sort $ toFrequencies xs) . toFrequencies

  where toFrequencies xs = map (\ys -> length $ filter ((==) (length ys) . length) xs) xs
-}

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
  properties (Problem.lsort, Problem.lfsort) ("lsort", "lfsort")
--  lsortProp Problem.lsort "lsort"
--  lfsortProp Problem.lfsort "lfsort"
  examples
  describe "From solutions" $ do
    properties (Solution.lsort, Solution.lfsort) ("lsort", "lfsort")
--    lsortProp Solution.lsort "lsort"
--    lfsortProp Solution.lfsort "lfsort"
