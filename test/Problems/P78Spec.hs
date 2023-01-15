{-|
Copyright: Copyright (C) 2022 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P78Spec (spec) where

import qualified Problems.P78          as Problem
import qualified Solutions.P78         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Integer) -> String -> Spec
properties collatz name = describe name $ do
  prop "is zero for one" $ do
    collatz 1 `shouldBe` 0

  prop "is one more than number of steps for next number in sequence" $ \(Positive n) ->
    n > 1 ==>
    collatz n `shouldBe` 1 + collatz (if odd n then 3*n+1 else n `div` 2)

examples :: Spec
examples = describe "Examples" $ do
  it "collatz 1" $ do
    collatz 1 `shouldBe` 0

  it "collatz 2" $ do
    collatz 2 `shouldBe` 1

  it "collatz 31" $ do
    collatz 31 `shouldBe` 106

  where collatz = Problem.collatz :: Int -> Int

spec :: Spec
spec = parallel $ do
  properties Problem.collatz "collatz"
  examples
  describe "From solutions" $ do
    properties Solution.collatz "collatz"
