{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P04Spec (spec) where

import qualified Problems.P04          as Problem
import qualified Solutions.P04         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck

properties :: ([Int] -> Int) -> String -> Spec
properties myLength name = describe name $ do
  it "returns zero for empty list" $ do
    myLength [] `shouldBe` 0

  prop "satisfies induction step" $
    \l -> myLength (1:l) `shouldBe` 1 + myLength l

examples :: Spec
examples = describe "Examples" $ do
  it "myLength [123, 456, 789]" $ do
    myLength [123, 456, 789 :: Int] `shouldBe` 3

  it "myLength \"Hello, world!\"" $ do
    myLength "Hello, world!" `shouldBe` 13

  where myLength = Problem.myLength

spec :: Spec
spec = parallel $ do
  properties Problem.myLength "myLength"
  examples
  describe "From solutions" $ do
    properties Solution.myLength    "myLength"
    properties Solution.myLength'   "myLength'"
    properties Solution.myLength''  "myLength''"
    properties Solution.myLength''' "myLength'''"
