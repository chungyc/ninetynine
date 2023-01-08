{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P32Spec (spec) where

import qualified Problems.P32          as Problem
import qualified Solutions.P32         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: (Integer -> Integer -> Integer) -> String -> Spec
properties myGCD name = describe name $ do
  prop "divides both integers" $
    \(NonZero a) -> \(NonZero b) ->
      myGCD a b `shouldSatisfy` \c -> a `mod` c == 0 && b `mod` c == 0

  prop "has no greater common divisor" $
    \(NonZero a) -> \(NonZero b) ->
      [(1 + myGCD a b)..(min (abs a) (abs b))]
      `shouldSatisfy` all (\k -> a `mod` k /= 0 || b `mod` k /= 0)

examples :: Spec
examples = describe "Examples" $ do
  it "myGCD 36 63" $ do
    myGCD 36 63 `shouldBe` 9

  it "myGCD 125 81" $ do
    myGCD 125 81 `shouldBe` 1

  it "myGCD 221 559" $ do
    myGCD 221 559 `shouldBe` 13

  where myGCD a b = Problem.myGCD a b :: Int

spec :: Spec
spec = parallel $ do
  properties Problem.myGCD "myGCD"
  examples
  describe "From solutions" $ do
    properties Solution.myGCD "myGCD"
