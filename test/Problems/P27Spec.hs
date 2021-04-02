{-|
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P27Spec (spec) where

import           Control.Monad
import           Data.List             (sort)
import qualified Data.Set              as Set
import qualified Problems.P27          as Problem
import qualified Solutions.P27         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> [Int] -> [[[Int]]]) -> String -> Spec
properties disjointGroups name = describe name $ modifyMaxSize (const 10) $ do
  prop "has expected sizes of groups" $
    \(List xs) -> \(GroupSizes ns) ->
      disjointGroups ns xs `shouldSatisfy` all ((==) ns . map length)

  prop "are disjoint groups of list" $
    \(List xs) -> \(GroupSizes ns) ->
      disjointGroups ns xs `shouldSatisfy` all ((==) (sort xs) . sort . concat)

  prop "is list of distinct groups" $
    \(List xs) -> \(GroupSizes ns) ->
      disjointGroups ns xs `shouldSatisfy`
      \l -> length l == Set.size (Set.fromList $ map (map sort) l)

  prop "has expected number of groups" $
    \(List xs) -> \(GroupSizes ns) ->
      disjointGroups ns xs `shouldSatisfy` (==) (multinomial ns) . length

  -- Assumes that the sum of group sizes exactly matches the length of the list.
  -- What happens when they do not is left undefined.

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

multinomial :: [Int] -> Int
multinomial ns = fromIntegral $ (factorial $ sum ns') `div` (product $ map factorial ns')
  where ns' = map toInteger ns :: [Integer]

factorial :: Integer -> Integer
factorial n = product [1..n]

-- | Generate list with length exactly equal to the size.
newtype List = List [Int] deriving (Eq, Show)

instance Arbitrary List where
  arbitrary = sized genList

genList :: Int -> Gen List
genList 0 = genList 1  -- no empty lists
genList n = return $ List [1..n]

-- | Generate list of group sizes that sum up exactly to the size.
newtype GroupSizes = GroupSizes [Int] deriving (Eq, Show)

instance Arbitrary GroupSizes where
  arbitrary = sized $ \n ->
    case n of 0  -> return $ GroupSizes [1]  -- no empty lists
              n' -> genGroupSizes n'

genGroupSizes :: Int -> Gen GroupSizes
genGroupSizes 0 = return $ GroupSizes []
genGroupSizes 1 = return $ GroupSizes [1]
genGroupSizes n = do
  k <- chooseInt (1, n)
  frequency [ (1, return $ GroupSizes $ end k)
            , (2, more k)
            ]
  where end k | k == n    = [k]
              | otherwise = [k, n-k]
        more k | k == n    = return $ GroupSizes [k]
               | otherwise = liftM (consGroupSizes k) $ genGroupSizes (n-k)

consGroupSizes :: Int -> GroupSizes -> GroupSizes
consGroupSizes x (GroupSizes xs) = GroupSizes (x:xs)
