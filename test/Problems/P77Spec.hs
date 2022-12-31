{-|
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P77Spec (spec) where

import           Data.List             (sort)
import qualified Problems.P77          as Problem
import qualified Solutions.P77         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> [[Int]]) -> String -> Spec
properties randomWalkPaths name =
  describe name $ modifyMaxSuccess (const 10) $ modifyMaxSize (const 10) $ do

  prop "starts paths with 0" $
    \(NonNegative n) -> randomWalkPaths n `shouldSatisfy` all ((==) 0 . head)

  prop "steps changes position by one of [-1,0,1]" $
    \(NonNegative n) -> randomWalkPaths n `shouldSatisfy`
                        all (\l -> all (\x -> elem x [-1,0,1]) $ map (uncurry (-)) $ zip (tail l) l)

  prop "has paths with expected length" $
    \(NonNegative n) -> randomWalkPaths n `shouldSatisfy` all ((==) (n+1) . length)

  prop "includes any random walk path" $ withMaxSuccess 100 $
    \(NonNegative n) -> forAll (pathWith n) $ \p -> randomWalkPaths n `shouldSatisfy` elem p

examples :: Spec
examples = do
  describe "Examples" $ do
    it "randomWalkPaths 0" $ do
      randomWalkPaths 0 `shouldBe` [[0]]

    it "sort $ randomWalkPaths 2" $ do
      (sort $ randomWalkPaths 2) `shouldBe`
        [[0,-1,-2],[0,-1,-1],[0,-1,0],[0,0,-1],[0,0,0],[0,0,1],[0,1,0],[0,1,1],[0,1,2]]

    where randomWalkPaths = Problem.randomWalkPaths

spec :: Spec
spec = parallel $ do
  properties Problem.randomWalkPaths "randomWalkPaths"
  examples
  describe "From solutions" $ do
    properties Solution.randomWalkPaths "randomWalkPaths"

-- Given the list of steps and path, return the path with the next path.
-- The list of steps are in order, but the path is in the reverse order of positions.
walk :: [Int] -> [Int] -> [Int]
walk [] path                      = path
walk _ []                         = []
walk (step:steps) path@(latest:_) = walk steps (latest+step : path)

-- Generator for a random walk path with n steps.
pathWith :: Int -> Gen [Int]
pathWith n = do
  steps <- vectorOf n $ elements[-1,0,1]
  return $ reverse $ walk steps [0]
