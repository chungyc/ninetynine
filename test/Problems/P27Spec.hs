{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P27Spec (spec) where

import           Data.List             (sort)
import qualified Problems.P27          as Problem
import qualified Solutions.P27         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> [Int] -> [[[Int]]]) -> String -> Spec
properties disjointGroups name = describe name $ do
  prop "has expected group sizes" $
    forAll (smallNumberList) $ \ns ->
    forAll (vectorOf (sum ns) arbitrary) $ \xs ->
    disjointGroups ns xs `shouldSatisfy` all ((==) ns . map length)

  prop "each grouping makes up original list" $
    forAll (smallNumberList) $ \ns ->
    forAll (vectorOf (sum ns) arbitrary) $ \xs ->
    disjointGroups ns xs `shouldSatisfy` all ((==) (sort xs) . sort . concat)

  prop "has expected number of groupings" $
    forAll (smallNumberList) $ \ns ->
    forAll (vectorOf (sum ns) arbitrary) $ \xs ->
    disjointGroups ns xs `shouldSatisfy` (==) (multinomial ns) . length

  prop "includes arbitrary groupings" $
    forAll (resize 5 $ listOf $ resize 5 $ listOf arbitrary) $ \gs ->
    not (tooLarge $ map length gs) ==>
    disjointGroups (map length gs) (concat gs)
    `shouldSatisfy` elem (sort $ map sort gs) . sortGroupings

  -- Assumes that the sum of group sizes exactly matches the length of the list.
  -- What happens when they do not is left undefined.

  where
    -- Sort a list of groupings so that every list at every level is sorted.
    sortGroupings groupings = sort $ map (sort . map sort) groupings

examples :: Spec
examples = describe "Examples" $ do
  it "head $ sort $ map (map sort) $ disjointGroups [2,3,4] names" $ do
    head (sort $ map (map sort) $ disjointGroups [2,3,4] names) `shouldBe`
      [["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]]

  it "length $ disjointGroups [2,3,4] names" $ do
    length (disjointGroups [2,3,4] names) `shouldBe` 1260

  it "length $ disjointGroups [2,2,5] names" $ do
    length (disjointGroups [2,2,5] names) `shouldBe` 756

  where disjointGroups = Problem.disjointGroups
        names = ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]

spec :: Spec
spec = parallel $ do
  properties Problem.disjointGroups "disjointGroups"
  examples
  describe "From solutions" $ do
    properties Solution.disjointGroups "disjointGroups"

-- | Generates a short list of small numbers to be used as group sizes.
--
-- It will not generate group sizes whose number of groupings are much larger
-- than the the test should handle.
smallNumberList :: Gen [Int]
smallNumberList = (resize 6 . listOf) smallNumber `suchThat` (not . tooLarge)

-- | Generates a small number for inclusion in group sizes.
smallNumber :: Gen Int
smallNumber = resize 6 $ arbitrarySizedNatural

-- | Whether a list of groups sizes will result in a number of groupings
-- larger than we should handle.  For preventing having to deal with too
-- many groupings in the test without unduly restricting the group sizes.
tooLarge :: [Int] -> Bool
tooLarge ns = multinomial ns > 1000

-- | Multinomial coefficient.
multinomial :: [Int] -> Int
multinomial ns = fromIntegral multinomial'
  where multinomial' = factorial (sum ns') `div` product (map factorial ns')
        ns' = map toInteger ns :: [Integer]  -- To accommodate large factors.
        factorial n = product [1..n]
