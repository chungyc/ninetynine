{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P17Spec (spec) where

import qualified Problems.P17          as Problem
import qualified Solutions.P17         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Int -> ([Int], [Int])) -> String -> Spec
properties split name = describe name $ do
  prop "splits nothing" $
    \(NonNegative n) -> split [] n `shouldBe` ([], [])

  prop "splits list" $
    \xs -> \ys -> split (xs ++ ys) (length xs) `shouldBe` (xs, ys)

  prop "does not split list with large enough given length" $
    \xs -> \(NonNegative n) -> split xs (n + length xs) `shouldBe` (xs, [])

examples :: Spec
examples = describe "Examples" $ do
  it "split \"abcdefghik\" 3" $ do
    split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

  where split = Problem.split

spec :: Spec
spec = parallel $ do
  properties Problem.split "split"
  examples
  describe "From solutions" $ do
    properties Solution.split "split"
