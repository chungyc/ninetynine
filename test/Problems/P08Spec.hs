{-|
Copyright: Copyright (C) 2023 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org
-}
module Problems.P08Spec (spec) where

import qualified Problems.P08          as Problem
import qualified Solutions.P08         as Solution
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> [Int]) -> String -> Spec
properties compress name = describe name $ do
  it "compresses nothing" $ do
    compress [] `shouldBe` []

  prop "compresses singleton" $ withMaxSuccess 10 $
    \x -> compress [x] `shouldBe` [x]

  prop "is idempotent" $
    \xs -> compress (compress xs) `shouldBe` compress xs

  prop "does not discard element" $
    \xs -> \x -> \xs' -> compress (xs ++ [x] ++ xs') `shouldSatisfy` elem x

  prop "removes consecutive duplicates" $
    \xs -> \(Positive k) -> \x -> \ys ->
      length xs == 0 || last xs /= x ==>
      length ys == 0 || head ys /= x ==>
      compress (xs ++ replicate k x ++ ys)
      `shouldBe` compress xs ++ [x] ++ compress ys

  prop "maintains order" $
    \xs -> \xs' ->
      length xs > 0 ==>
      length xs' > 0 ==>
      disjoin [ last xs /= head xs ==>
                compress (xs ++ xs') `shouldBe` compress xs ++ compress xs'
              , last xs == head xs ==>
                compress (xs ++ xs') `shouldBe` compress xs ++ tail (compress xs')
              ]

examples :: Spec
examples = do
  describe "Examples" $ do
    it "compress \"aaaabccaadeeee\"" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"

  where compress = Problem.compress

spec :: Spec
spec = parallel $ do
  properties Problem.compress "compress"
  examples
  describe "From solutions" $ do
    properties Solution.compress "compress"
