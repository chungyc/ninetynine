{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P22Spec (spec) where

import qualified Problems.P22          as Problem
import qualified Solutions.P22         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Int -> Int -> [Int]) -> String -> Spec
properties range name = describe name $ do
  prop "contains single number" $ \n ->
    range n n `shouldMatchList` [n]

  prop "extends range by one" $ \n (NonNegative k) ->
    range n (n+k+1) `shouldMatchList` range n (n+k) ++ [n+k+1]

  prop "comprised of sub-ranges" $ \n (NonNegative k) (Positive l) ->
    range n (n+k+l) `shouldMatchList` range n (n+k) ++ range (n+k+1) (n+k+l)

examples :: Spec
examples = describe "Examples" $ do
  it "range 4 9" $ do
    range 4 9 `shouldMatchList` [4,5,6,7,8,9]

  where range = Problem.range

spec :: Spec
spec = parallel $ do
  properties Problem.range "range"
  examples
  describe "From solutions" $ do
    properties Solution.range "range"
