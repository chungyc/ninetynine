{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P26Spec (spec) where

import           Data.List             (isSubsequenceOf, sort)
import qualified Problems.P26          as Problem
import qualified Solutions.P26         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> [Int] -> [[Int]]) -> String -> Spec
properties combinations name = describe name $ do
  modifyMaxSize (const 20) $ do
    prop "are of expected length" $ \xs ->
      forAll (chooseInt (0, length xs)) $ \n ->
      combinations n xs `shouldSatisfy` all ((==) n . length)

    prop "are combinations" $ \xs ->
      forAll (chooseInt (0, length xs)) $ \n ->
      combinations n xs `shouldSatisfy` all (flip isSubsequenceOf (sort xs) . sort)

    prop "includes arbitrary combinations" $ \xs ->
      forAll (sublistOf xs) $ \xs' ->
      combinations (length xs') xs `shouldSatisfy` elem (sort xs') . map sort

examples :: Spec
examples = describe "Examples" $ do
  it "length $ combinations 3 [1..12]" $ do
    length (combinations 3 [1..12 :: Int]) `shouldBe` 220

  it "combinations 3 \"abcdef\"" $ do
    combinations 3 "abcdef" `shouldMatchList`
      ["abc","abd","abe","abf","acd","ace","acf","ade","adf","aef"
      ,"bcd","bce","bcf","bde","bdf","bef","cde","cdf","cef","def"
      ]

  where combinations = Problem.combinations

spec :: Spec
spec = parallel $ do
  properties Problem.combinations "combinations"
  examples
  describe "From solutions" $ do
    properties Solution.combinations "combinations"
