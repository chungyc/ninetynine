{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

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
  prop "compresses nothing" $
    compress [] `shouldBe` []

  prop "compresses singleton" $ \x ->
    compress [x] `shouldBe` [x]

  prop "is idempotent" $ \xs ->
    compress (compress xs) `shouldBe` compress xs

  prop "does not discard element" $ \xs x xs' ->
    compress (xs ++ [x] ++ xs') `shouldSatisfy` elem x

  prop "removes consecutive duplicates" $
    \xs (Positive k) x ys ->
      null xs || last xs /= x ==>
      null ys || head ys /= x ==>
      compress (xs ++ replicate k x ++ ys)
      `shouldBe` compress xs ++ [x] ++ compress ys

  prop "maintains order" $ \xs xs' ->
    not (null xs) ==>
    not (null xs') ==>
    counterexample (show $ xs ++ xs') $
    case last xs == head xs' of
      True -> compress (xs ++ xs') `shouldBe` compress xs ++ tail (compress xs')
      False -> compress (xs ++ xs') `shouldBe` compress xs ++ compress xs'

examples :: Spec
examples = describe "Examples" $ do
  it "compress \"aaaabccaadeeee\"" $ do
    compress "aaaabccaadeeee" `shouldBe` "abcade"

  where compress = Problem.compress

spec :: Spec
spec = parallel $ do
  properties Problem.compress "compress"
  examples
  describe "From solutions" $ do
    properties Solution.compress "compress"
