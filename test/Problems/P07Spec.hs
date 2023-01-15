{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P07Spec (spec) where

import           Problems.Lists
import           Problems.Lists.Arbitrary ()
import qualified Problems.P07             as Problem
import qualified Solutions.P07            as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: (NestedList Int -> [Int]) -> String -> Spec
properties flatten name = describe name $ do
  prop "singleton is singleton" $ \x ->
    flatten (Elem x) `shouldBe` [x]

  prop "flat list is flat" $ \xs ->
    flatten (List $ map Elem xs) `shouldBe` xs

  prop "concatenates flattened items" $ \xs ->
    flatten (List xs) `shouldBe` concatMap flatten xs

examples :: Spec
examples = describe "Examples" $ do
  it "flatten (Elem 5)" $ do
    flatten (Elem 5) `shouldBe` [5 :: Int]

  it "flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])" $ do
    flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
      `shouldBe` [1,2,3,4,5 :: Int]

  it "flatten (List [])" $ do
    flatten (List []) `shouldBe` ([] :: [Int])

  where flatten = Problem.flatten

spec :: Spec
spec = parallel $ do
  properties Problem.flatten "flatten"
  examples
  describe "From solutions" $ do
    properties Solution.flatten  "flatten"
    properties Solution.flatten' "flatten'"
