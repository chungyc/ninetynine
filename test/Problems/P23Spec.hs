{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P23Spec (spec) where

import           Data.List             (isSubsequenceOf, sort, unfoldr)
import qualified Problems.P23          as Problem
import qualified Solutions.P23         as Solution
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Int -> StdGen -> ([Int], StdGen)) -> String -> Spec
properties randomSelect name = describe name $ do
  prop "selects given number of elements" $ \xs ->
    forAll (chooseInt (0, length xs)) $ \n seed ->
    randomSelect xs n (mkStdGen seed) `shouldSatisfy` (==) n . length . fst

  prop "selects elements from list" $ \xs ->
    forAll (chooseInt (0, length xs)) $ \n seed ->
    randomSelect xs n (mkStdGen seed)
    `shouldSatisfy` flip isSubsequenceOf (sort xs) . sort . fst

  prop "is random and returns new random generator" $ once $ \seed ->
    -- Make a number of selections and confirm that the selection is random by
    -- checking at least one of them is different from another.
    -- It is theoretically possible for all of them to be the same with
    -- true random numbers, but it is vanishingly unlikely.
    --
    -- Similarly, this also tests that randomSelect returns a new random generator.
    -- If it did not, the use of the same generator would return identical selections.
    let selections = unfoldr (Just . randomSelect [1..100] 10) $ mkStdGen seed
        isRandom ls = any (uncurry (/=)) $ zip ls $ tail ls
    in conjoin [ selections `shouldSatisfy` any isRandom
               , selections `shouldSatisfy` isRandom
               ]

examples :: Spec
examples = describe "Examples" $ do
  it "fst $ randomSelect \"abcdefgh\" 3 $ mkStdGen 111" $ do
    fst (randomSelect "abcdefgh" 3 $ mkStdGen 111)
      `shouldSatisfy` \l -> sort l `isSubsequenceOf` "abcdefgh" && length l == 3

  it "take 5 $ unfoldr (Just . randomSelect [1..100] 3) $ mkStdGen 111" $ do
    take 5 (unfoldr (Just . randomSelect [1..100 :: Int] 3) $ mkStdGen 111)
      `shouldSatisfy` all (\l -> sort l `isSubsequenceOf` [1..100] && length l == 3)

  it "newStdGen >>= return . fst . randomSelect \"abcdefgh\" 3" $ do
    newStdGen >>=
      (`shouldSatisfy` \l -> sort l `isSubsequenceOf` "abcdefgh" && length l == 3)
      . fst . randomSelect "abcdefgh" 3

  where randomSelect l n g = Problem.randomSelect l n g

spec :: Spec
spec = parallel $ do
  properties Problem.randomSelect "randomSelect"
  examples
  describe "From solutions" $ do
    properties Solution.randomSelect "randomSelect"
