{-|
Copyright: Copyright (C) 2023 Yoo Chung
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
properties randomWalkPaths name = describe name $
  modifyMaxSuccess (const 10) $
  modifyMaxSize (const 10) $ do

  prop "has single path with zero steps" $
    randomWalkPaths 0 `shouldBe` [[0]]

  prop "changes position by one of [-1, 0, 1]" $ \(Positive n) ->
    randomWalkPaths n `shouldMatchList`
    concatMap
    (\path -> [ path ++ [last path + step] | step <- [-1,0,1] ])
    (randomWalkPaths $ n-1)

  prop "has paths with expected length" $
    \(NonNegative n) -> randomWalkPaths n `shouldSatisfy` all ((==) (n+1) . length)

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
